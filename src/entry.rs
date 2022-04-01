use proc_macro::TokenStream;
use proc_macro2::{Span};

use quote::{quote, quote_spanned, ToTokens};
use syn::parse::Parser;
type AttributeArgs = syn::punctuated::Punctuated<syn::NestedMeta, syn::Token![,]>;

fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
}

struct FinalConfig {
    reload_max_times: Option<usize>,
    monitor_file: Option<String>,
}

const DEFAULT_ERROR_CONFIG: FinalConfig = FinalConfig {
    reload_max_times: None,
    monitor_file: None,
};

struct Configuration {
    reload_max_times: Option<(usize, Span)>,
    monitor_file: Option<(String, Span)>,
}


fn parse_int(int: syn::Lit, span: Span, field: &str) -> Result<usize, syn::Error> {
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

impl Configuration {
    fn new() -> Self {
        Configuration {
            reload_max_times: None,
            monitor_file: None,
        }
    }

    fn set_reload_max_times(
        &mut self,
        reload_max_times: syn::Lit,
        span: Span,
    ) -> Result<(), syn::Error> {
        if self.reload_max_times.is_some() {
            return Err(syn::Error::new(
                span,
                "`reload_max_times` set multiple times.",
            ));
        }

        let reload_max_times = parse_int(reload_max_times, span, "reload_max_times")?;
        if reload_max_times == 0 {
            return Err(syn::Error::new(span, "`reload_max_times` may not be 0."));
        }
        self.reload_max_times = Some((reload_max_times, span));
        Ok(())
    }
    
    fn set_monitor_file(
        &mut self,
        monitor_file: syn::Lit,
        span: Span,
    ) -> Result<(), syn::Error> {
        if self.monitor_file.is_some() {
            return Err(syn::Error::new(
                span,
                "`monitor_file` set multiple times.",
            ));
        }

        let monitor_file = parse_string(monitor_file, span, "reload_max_times")?;
        if monitor_file == String::new() {
            return Err(syn::Error::new(span, "`monitor_file` may not be empty."));
        }
        self.monitor_file = Some((monitor_file, span));
        Ok(())
    }

    fn build(&self) -> Result<FinalConfig, syn::Error> {
        let reload_max_times = match self.reload_max_times {
            None => None,
            Some((value, _)) => {
                Some(value)
            }
        };
        
        let monitor_file = match &self.monitor_file {
            None => None,
            Some((value, _)) => {
                Some(value.clone())
            }
        };

        Ok(FinalConfig {
            reload_max_times,
            monitor_file,
        })
    }
}

fn build_config(
    _input: syn::ItemFn,
    args: AttributeArgs,
) -> Result<FinalConfig, syn::Error> {
    let mut config = Configuration::new();
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
                    "max_times" => {
                        config.set_reload_max_times(
                            namevalue.lit.clone(),
                            syn::spanned::Spanned::span(&namevalue.lit),
                        )?;
                    }
                    "monitor_file" => {
                        config.set_monitor_file(
                            namevalue.lit.clone(),
                            syn::spanned::Spanned::span(&namevalue.lit),
                        )?;
                    }
                    name => {
                        let msg = format!(
                            "Unknown attribute {} is specified; expected one of: `flavor`, `reload_max_times`, `start_paused`",
                            name,
                        );
                        return Err(syn::Error::new_spanned(namevalue, msg));
                    }
                }
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


fn parse_knobs(mut input: syn::ItemFn, config: FinalConfig) -> TokenStream {
    let (_last_stmt_start_span, last_stmt_end_span) = {
        let mut last_stmt = input
            .block
            .stmts
            .last()
            .map(ToTokens::into_token_stream)
            .unwrap_or_default()
            .into_iter();
        let start = last_stmt.next().map_or_else(Span::call_site, |t| t.span());
        let end = last_stmt.last().map_or(start, |t| t.span());
        (start, end)
    };
    let reload_max_times = config.reload_max_times.unwrap_or(9999999);
    let monitor_file_name = config.monitor_file.unwrap_or(String::new());
    let body = &input.block;
    

    input.block = syn::parse2(quote_spanned! {last_stmt_end_span=>
        {   
            {
                let command = ::commander::Commander::new()
                .usage_desc("Forever Write by Rust")
                .option("-ff, --fromforever [value]", "fromforever ", Some(false))
                .option_str("-fm, --monitor [value]", "monitor file change for update", None)
                .helps(vec!["fh".to_string(), "forever-help".to_string()])
                .versions(vec!["fv".to_string(), "forever-version".to_string()])
                .after_desc("\n\n Forever run in rust\n\n")
                .parse_env_or_exit();
    
                let mut reload_left_times = #reload_max_times;
                let mut monitor_file = #monitor_file_name.to_string();
                if let Some(monitor) = command.get_str("monitor") {
                    monitor_file = monitor;
                }
    
                if !command.get("fromforever").unwrap_or(false) {
                    let mut list = command.get_all_args();
                    list.push("--fromforever".to_string());
                    loop {
                        reload_left_times-=1;
                        if reload_left_times <= 0 {
                            break
                        }
    
                        let now = ::std::time::SystemTime::now();
                        let mut child = if cfg!(target_os = "windows") {
                            ::std::process::Command::new(command.get_exec().unwrap())
                                    .args(list.clone())
                                    .stdin(::std::process::Stdio::null())
                                    .stdout(::std::process::Stdio::inherit())
                                    .spawn()
                                    .expect("failed to execute process")
                        } else {
                            ::std::process::Command::new(command.get_exec().unwrap())
                                    .args(list.clone())
                                    .stdin(::std::process::Stdio::null())
                                    .stdout(::std::process::Stdio::inherit())
                                    .spawn()
                                    .expect("failed to execute process")
                        };
    
                        loop {
                            match child.try_wait() {
                                Ok(Some(status)) => {println!("exited with: {}", status);break;},
                                Ok(None) => {
                                }
                                Err(e) => {println!("error attempting to wait: {}", e); break;},
                            }
                            ::std::thread::sleep(::std::time::Duration::from_millis(1000));
                            if monitor_file != String::new() {
                                if let Some(metadata) = ::std::fs::metadata(&monitor_file).ok() {
                                    if let Ok(time) = metadata.modified() {
                                        let result = time.cmp(&now);
                                        if result == ::std::cmp::Ordering::Greater {
                                            child.kill().ok();
                                            break;
                                        }
                                    }
                                }
                            } 
                        }
                    }
                    return
                }
            }

            #body
        }
    }).expect("Parsing failure");

    let result = quote! {
        #input
    };
    result.into()
}

// #[cfg(not(test))] // Work around for rust-lang/rust#62127
pub fn main(args: TokenStream, item: TokenStream) -> TokenStream {

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
            .and_then(|args| build_config(input.clone(), args))
    };

    match config {
        Ok(config) => parse_knobs(input, config),
        Err(e) => token_stream_with_error(parse_knobs(input, DEFAULT_ERROR_CONFIG), e),
    }


}