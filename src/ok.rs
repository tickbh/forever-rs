use proc_macro::TokenStream;
use proc_macro2::{Span, Ident};

use quote::{quote, quote_spanned, ToTokens};
use syn::parse::Parser;
type AttributeArgs = syn::punctuated::Punctuated<syn::NestedMeta, syn::Token![,]>;

fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
}

struct FinalConfig {
    worker_threads: Option<usize>,
}

const DEFAULT_ERROR_CONFIG: FinalConfig = FinalConfig {
    worker_threads: None,
};

struct Configuration {
    worker_threads: Option<(usize, Span)>,
    is_test: bool,
}


fn parse_int(int: syn::Lit, span: Span, field: &str) -> Result<usize, syn::Error> {
    println!("int === {:?}", span);
    println!("field === {:?}", field);
    match int {
        syn::Lit::Int(lit) => match lit.base10_parse::<usize>() {
            Ok(value) => Ok(value),
            Err(e) => Err(syn::Error::new(
                span,
                format!("Failed to parse value of `{}` as integer: {}", field, e),
            )),
        },
        _ => Err(syn::Error::new(
            span,
            format!("Failed to parse value of `{}` as integer.", field),
        )),
    }
}

fn parse_string(int: syn::Lit, span: Span, field: &str) -> Result<String, syn::Error> {
    match int {
        syn::Lit::Str(s) => Ok(s.value()),
        syn::Lit::Verbatim(s) => Ok(s.to_string()),
        _ => Err(syn::Error::new(
            span,
            format!("Failed to parse value of `{}` as string.", field),
        )),
    }
}

fn parse_bool(bool: syn::Lit, span: Span, field: &str) -> Result<bool, syn::Error> {
    match bool {
        syn::Lit::Bool(b) => Ok(b.value),
        _ => Err(syn::Error::new(
            span,
            format!("Failed to parse value of `{}` as bool.", field),
        )),
    }
}


impl Configuration {
    fn new(is_test: bool) -> Self {
        Configuration {
            worker_threads: None,
            is_test,
        }
    }


    fn set_worker_threads(
        &mut self,
        worker_threads: syn::Lit,
        span: Span,
    ) -> Result<(), syn::Error> {
        if self.worker_threads.is_some() {
            return Err(syn::Error::new(
                span,
                "`worker_threads` set multiple times.",
            ));
        }

        let worker_threads = parse_int(worker_threads, span, "worker_threads")?;
        if worker_threads == 0 {
            return Err(syn::Error::new(span, "`worker_threads` may not be 0."));
        }
        self.worker_threads = Some((worker_threads, span));
        Ok(())
    }

    fn macro_name(&self) -> &'static str {
        if self.is_test {
            "forever::test"
        } else {
            "forever::main"
        }
    }

    fn build(&self) -> Result<FinalConfig, syn::Error> {

        let worker_threads = match self.worker_threads {
            None => None,
            Some((value, _)) => {
                Some(value)
            }
        };

        Ok(FinalConfig {
            worker_threads,
        })
    }
}

fn build_config(
    input: syn::ItemFn,
    args: AttributeArgs,
    is_test: bool,
) -> Result<FinalConfig, syn::Error> {
    // if input.sig.asyncness.is_none() {
    //     let msg = "the `async` keyword is missing from the function declaration";
    //     return Err(syn::Error::new_spanned(input.sig.fn_token, msg));
    // }

    let mut config = Configuration::new(is_test);
    let macro_name = config.macro_name();

    for arg in args {
        match arg {
            syn::NestedMeta::Meta(syn::Meta::NameValue(namevalue)) => {
                let ident = namevalue
                    .path
                    .get_ident()
                    .ok_or_else(|| {
                        syn::Error::new_spanned(&namevalue, "Must have specified ident")
                    })?
                    .to_string()
                    .to_lowercase();
                match ident.as_str() {
                    "worker_threads" => {
                        config.set_worker_threads(
                            namevalue.lit.clone(),
                            syn::spanned::Spanned::span(&namevalue.lit),
                        )?;
                    }
                    name => {
                        let msg = format!(
                            "Unknown attribute {} is specified; expected one of: `flavor`, `worker_threads`, `start_paused`",
                            name,
                        );
                        return Err(syn::Error::new_spanned(namevalue, msg));
                    }
                }
            }
            syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                let name = path
                    .get_ident()
                    .ok_or_else(|| syn::Error::new_spanned(&path, "Must have specified ident"))?
                    .to_string()
                    .to_lowercase();
                let msg = match name.as_str() {
                    "threaded_scheduler" | "multi_thread" => {
                        format!(
                            "Set the runtime flavor with #[{}(flavor = \"multi_thread\")].",
                            macro_name
                        )
                    }
                    "basic_scheduler" | "current_thread" | "single_threaded" => {
                        format!(
                            "Set the runtime flavor with #[{}(flavor = \"current_thread\")].",
                            macro_name
                        )
                    }
                    "flavor" | "worker_threads" | "start_paused" => {
                        format!("The `{}` attribute requires an argument.", name)
                    }
                    name => {
                        format!("Unknown attribute {} is specified; expected one of: `flavor`, `worker_threads`, `start_paused`", name)
                    }
                };
                return Err(syn::Error::new_spanned(path, msg));
            }
            other => {
                return Err(syn::Error::new_spanned(
                    other,
                    "Unknown attribute inside the macro",
                ));
            }
        }
    }

    config.build()
}


fn parse_knobs(mut input: syn::ItemFn, is_test: bool, config: FinalConfig) -> TokenStream {
    // input.sig.asyncness = None;

    // // If type mismatch occurs, the current rustc points to the last statement.
    let (last_stmt_start_span, last_stmt_end_span) = {
        let mut last_stmt = input
            .block
            .stmts
            .last()
            .map(ToTokens::into_token_stream)
            .unwrap_or_default()
            .into_iter();
        // `Span` on stable Rust has a limitation that only points to the first
        // token, not the whole tokens. We can work around this limitation by
        // using the first/last span of the tokens like
        // `syn::Error::new_spanned` does.
        let start = last_stmt.next().map_or_else(Span::call_site, |t| t.span());
        let end = last_stmt.last().map_or(start, |t| t.span());
        (start, end)
    };
    let worker_threads = config.worker_threads.unwrap_or(0);
    let body = &input.block;
    input.block = syn::parse2(quote_spanned! {last_stmt_end_span=>
        {
            let command = ::commander::Commander::new()
            .usage_desc("Forever Write by Rust")
            .option("-ff, --fromforever [value]", "fromforever ", Some(false))
            .option_str("-fm, --monitor [value]", "monitor file change for update", None)
            .helps(vec!["fh".to_string(), "forever-help".to_string()])
            .versions(vec!["fv".to_string(), "forever-version".to_string()])
            .after_desc("\n\n Forever run in rust\n\n")
            .parse_env_or_exit();

            let args = env::args();
            let mut list = command.get_all_args();
            
            println!("command.get_exec() == {:?}", command.get_exec());
            println!("command.get_all_args() == {:?}", command.get_all_args());

            println!("111 == {:?}", command.get("fromforever"));
            let ssss = format!("command.get_exec() == {:?} command.get_all_args() == {:?}", command.get_exec(), command.get_all_args());
            
            if !command.get("fromforever").unwrap_or(false) {
                list.push("--fromforever".to_string());
                loop {
                    let mut now = ::std::time::SystemTime::now();
                    let mut child = if cfg!(target_os = "windows") {
                        Command::new(command.get_exec().unwrap())
                                .args(list.clone())
                                .stdin(Stdio::null())
                                .stdout(Stdio::inherit())
                                .spawn()
                                .expect("failed to execute process")
                    } else {
                        Command::new(command.get_exec().unwrap())
                                .args(list.clone())
                                .stdin(Stdio::null())
                                .stdout(Stdio::inherit())
                                .spawn()
                                .expect("failed to execute process")
                    };
                    println!("hello = {:?}", child);
                    println!("args = {:?}", list);
                    
                    loop {
                        
                        match child.try_wait() {
                            Ok(Some(status)) => {println!("exited with: {}", status);break;},
                            Ok(None) => {
                                println!("status not ready yet, let's really wait");
                            }
                            Err(e) => {println!("error attempting to wait: {}", e); break;},
                        }
                        ::std::thread::sleep_ms(1000);
                        if let Some(file) = command.get_str("monitor") {
                            // println!("ffffffffffff {:?}", ::std::fs::metadata(file));
                            if let Some(metadata) = ::std::fs::metadata(file).ok() {
                                println!("metadata == {:?}", metadata);
                                if let Ok(time) = metadata.modified() {
                                    let result = time.cmp(&now);
                                    println!("time === {:?} now == {:?}", time, now);
                                    println!("result === {:?}", result);
                                    if result == ::std::cmp::Ordering::Greater {
                                        child.kill();
                                        println!("killed!!!!!");
                                        break;
                                    }
                                }
                            }
                            println!("aaaaaaaaaa");
                        } 
                    }
                }
            }

            eprintln!("------?????????????---------- {:?}", #worker_threads);
            #body
        }
    }).expect("Parsing failure");

    let result = quote! {
        use std::io::prelude::*;
        #input
    };
    println!("result == {:?}", result);
    result.into()
}

// #[cfg(not(test))] // Work around for rust-lang/rust#62127
pub fn main(args: TokenStream, item: TokenStream) -> TokenStream {
    println!("args = {:?}", args);
    println!("input = {:?}", item);

    let input: syn::ItemFn = match syn::parse(item.clone()) {
        Ok(it) => it,
        Err(e) => return token_stream_with_error(item, e),
    };

    let config = if input.sig.ident == "main" && !input.sig.inputs.is_empty() {
        let msg = "the main function cannot accept arguments";
        Err(syn::Error::new_spanned(&input.sig.ident, msg))
    } else {
        AttributeArgs::parse_terminated
            .parse(args)
            .and_then(|args| build_config(input.clone(), args, false))
    };

    
    match config {
        Ok(config) => parse_knobs(input, false, config),
        Err(e) => token_stream_with_error(parse_knobs(input, false, DEFAULT_ERROR_CONFIG), e),
    }


}