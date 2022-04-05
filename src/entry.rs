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
    pid_file: Option<String>,
    cpid_file: Option<String>,
    log_file: Option<String>,
    error_file: Option<String>,
    is_daemon: Option<bool>,
}

const DEFAULT_ERROR_CONFIG: FinalConfig = FinalConfig {
    reload_max_times: None,
    monitor_file: None,
    pid_file: None,
    cpid_file: None,
    log_file: None,
    error_file: None,
    is_daemon: None,
};

struct Configuration {
    reload_max_times: Option<(usize, Span)>,
    monitor_file: Option<(String, Span)>,
    pid_file: Option<(String, Span)>,

    cpid_file: Option<(String, Span)>,
    log_file: Option<(String, Span)>,
    error_file: Option<(String, Span)>,
    is_daemon: Option<(bool, Span)>,
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
            pid_file: None,
            cpid_file: None,
            log_file: None,
            error_file: None,
            is_daemon: None,
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

    fn set_is_daemon(
        &mut self,
        is_daemon: syn::Lit,
        span: Span,
    ) -> Result<(), syn::Error> {
        if self.is_daemon.is_some() {
            return Err(syn::Error::new(
                span,
                "`is_daemon` set multiple times.",
            ));
        }

        let is_daemon = parse_bool(is_daemon, span, "is_daemon")?;
        self.is_daemon = Some((is_daemon, span));
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


    fn set_pid_file(
        &mut self,
        pid_file: syn::Lit,
        span: Span,
    ) -> Result<(), syn::Error> {
        if self.pid_file.is_some() {
            return Err(syn::Error::new(
                span,
                "`pid_file` set multiple times.",
            ));
        }

        let pid_file = parse_string(pid_file, span, "reload_max_times")?;
        if pid_file == String::new() {
            return Err(syn::Error::new(span, "`pid_file` may not be empty."));
        }
        self.pid_file = Some((pid_file, span));
        Ok(())
    }


    fn set_cpid_file(
        &mut self,
        cpid_file: syn::Lit,
        span: Span,
    ) -> Result<(), syn::Error> {
        if self.cpid_file.is_some() {
            return Err(syn::Error::new(
                span,
                "`cpid_file` set multiple times.",
            ));
        }

        let cpid_file = parse_string(cpid_file, span, "reload_max_times")?;
        if cpid_file == String::new() {
            return Err(syn::Error::new(span, "`cpid_file` may not be empty."));
        }
        self.cpid_file = Some((cpid_file, span));
        Ok(())
    }


    fn set_log_file(
        &mut self,
        log_file: syn::Lit,
        span: Span,
    ) -> Result<(), syn::Error> {
        if self.log_file.is_some() {
            return Err(syn::Error::new(
                span,
                "`log_file` set multiple times.",
            ));
        }

        let log_file = parse_string(log_file, span, "reload_max_times")?;
        if log_file == String::new() {
            return Err(syn::Error::new(span, "`log_file` may not be empty."));
        }
        self.log_file = Some((log_file, span));
        Ok(())
    }


    fn set_error_file(
        &mut self,
        error_file: syn::Lit,
        span: Span,
    ) -> Result<(), syn::Error> {
        if self.error_file.is_some() {
            return Err(syn::Error::new(
                span,
                "`error_file` set multiple times.",
            ));
        }

        let error_file = parse_string(error_file, span, "reload_max_times")?;
        if error_file == String::new() {
            return Err(syn::Error::new(span, "`error_file` may not be empty."));
        }
        self.error_file = Some((error_file, span));
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


        let pid_file = match &self.pid_file {
            None => None,
            Some((value, _)) => {
                Some(value.clone())
            }
        };

        let cpid_file = match &self.cpid_file {
            None => None,
            Some((value, _)) => {
                Some(value.clone())
            }
        };

        let log_file = match &self.log_file {
            None => None,
            Some((value, _)) => {
                Some(value.clone())
            }
        };


        let error_file = match &self.error_file {
            None => None,
            Some((value, _)) => {
                Some(value.clone())
            }
        };

        let is_daemon = match &self.is_daemon {
            None => None,
            Some((value, _)) => {
                Some(value.clone())
            }
        };

        Ok(FinalConfig {
            reload_max_times,
            monitor_file,
            pid_file,
            cpid_file,
            log_file,
            error_file,
            is_daemon,
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
                    "pid_file" => {
                        config.set_pid_file(
                            namevalue.lit.clone(),
                            syn::spanned::Spanned::span(&namevalue.lit),
                        )?;
                    }
                    "cpid_file" => {
                        config.set_cpid_file(
                            namevalue.lit.clone(),
                            syn::spanned::Spanned::span(&namevalue.lit),
                        )?;
                    }
                    "log_file" => {
                        config.set_log_file(
                            namevalue.lit.clone(),
                            syn::spanned::Spanned::span(&namevalue.lit),
                        )?;
                    }
                    "error_file" => {
                        config.set_error_file(
                            namevalue.lit.clone(),
                            syn::spanned::Spanned::span(&namevalue.lit),
                        )?;
                    }
                    "daemon" => {
                        config.set_is_daemon(
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
    let pid_file_name = config.pid_file.unwrap_or(String::new());
    let cpid_file_name = config.cpid_file.unwrap_or(String::new());
    let is_daemon = config.is_daemon.unwrap_or(false);
    let body = &input.block;
    input.block = syn::parse2(quote_spanned! {last_stmt_end_span=>
        {   
            {
                let command = ::commander::Commander::new()
                .usage_desc("Forever Write by Rust")
                .option("-fforever, --FromForever [value]", "FromForever ", Some(false))
                .option("-fstatus, --FromStatus [value]", "FromStatus ", None)
                .option("-fdamon, --FromDaemon [value]", "FromDaemon ", None)
                .option("-fstop, --FromStop [value]", "FromStop ", None)
                .option_str("-fmonotor, --monitor [value]", "monitor file change for update", None)
                .option_str("-fpid, --FromPid [value]", "pid file change for update", Some("forever.pid".to_string()))
                .option_str("-fcpid, --FromCPid [value]", "child pid file change for update", Some("child_forever.pid".to_string()))
                .helps(vec!["fh".to_string(), "forever-help".to_string()])
                .versions(vec!["fv".to_string(), "forever-version".to_string()])
                .after_desc("\n\n Forever run in rust\n\n")
                .parse_env_or_exit();
    
                let mut reload_left_times = #reload_max_times;
                let mut is_daemon = #is_daemon;
                let mut monitor_file = #monitor_file_name.to_string();
                let mut pid_file = #pid_file_name.to_string();
                let mut cpid_file = #cpid_file_name.to_string();
                if let Some(monitor) = command.get_str("monitor") {
                    monitor_file = monitor;
                }
                if let Some(pid) = command.get_str("FromPid") {
                    pid_file = pid;
                }
                if let Some(cpid) = command.get_str("FromCPid") {
                    cpid_file = cpid;
                }
                if let Some(daemon) = command.get("FromDaemon") {
                    is_daemon = daemon;
                }

                fn get_file_content(filename: String) -> String {
                    use std::io::prelude::*;
                    let mut file = ::std::fs::File::open(filename.to_string());
                    if !file.is_ok() {
                        // println!("pid file:'{}' no exist", filename);
                        return String::new();
                    }
                    let mut file_content = String::new();
                    file.unwrap().read_to_string(&mut file_content).expect("read pid file content failed");
                    return file_content;
                }

                fn kill_process_by_id(id: String) -> Option<i32> {
                    if id == String::new() {
                        return Some(-1);
                    }
                    let mut child = if cfg!(target_os = "windows") {
                        ::std::process::Command::new("kill")
                                .output()
                                .expect("failed to execute process")
                    } else {
                        ::std::process::Command::new("kill")
                                .args(["-TERM".to_string(), id.clone()])
                                .output()
                                .expect("failed to execute process")
                    };
                    println!("child = {:?}", child);
                    return child.status.code();
                }

                if let Some(stop) = command.get("FromStop") {
                    let pid_id = get_file_content(pid_file.clone());
                    match kill_process_by_id(pid_id.clone()) {
                        Some(0) => {
                            ::std::fs::remove_file(pid_file).ok();
                            println!("success close forever process:{}", pid_id);
                        },
                        Some(-1) => {
                            println!("please run first");
                            return;
                        },
                        _ => {
                            ::std::fs::remove_file(pid_file).ok();
                            println!("already close by other");
                        }
                    };

                    let cpid_id = get_file_content(cpid_file.clone());
                    match kill_process_by_id(cpid_id.clone()) {
                        Some(0) => {
                            ::std::fs::remove_file(cpid_file).ok();
                            println!("success close forever process:{}", cpid_id);
                        },
                        Some(-1) => {
                            return;
                        },
                        _ => {
                            ::std::fs::remove_file(cpid_file).ok();
                            println!("already close by other");
                        }
                    };
                    return;
                }


                if let Some(status) = command.get("FromStatus") {
                    let pid_id = get_file_content(pid_file.clone());

                    let mut child = if cfg!(target_os = "windows") {
                        ::std::process::Command::new(command.get_exec().unwrap())
                                .output()
                                .expect("failed to execute process")
                    } else {
                        ::std::process::Command::new("ps")
                                .args(["-p".to_string(), pid_id.clone()])
                                .output()
                                .expect("failed to execute process")
                    };

                    println!("hello == {:?}", child);

                    if child.status.code() != Some(0) {
                        println!("{} is dead", pid_id);
                        return;
                    }

                    let content = String::from_utf8(child.stdout).unwrap();
                    if let Some(_) = content.find(&pid_id) {
                        println!("{} alive", pid_id);
                    } else {
                        println!("{} is dead", pid_id);
                    }
                    return;
                }

                if is_daemon {
                    println!("is_daemon!!!!");
                    let mut list = command.get_all_args();
                    list.push("--FromDaemon".to_string());
                    list.push("false".to_string());
                    let child = ::std::process::Command::new(command.get_exec().unwrap())
                                .args(list.clone())
                                .stdin(::std::process::Stdio::inherit())
                                .stdout(::std::process::Stdio::inherit())
                                .spawn()
                                .expect("failed to execute process");
                    return;
                }

                // if command.get("FromDaemon").unwrap_or(false) {
                //     let mut list = command.get_all_args();
                //     list.retain(|&value| value != String::new("FromDaemon") && value != String::new("fd"))
                // }
    
                if !command.get("FromForever").unwrap_or(false) {
                    if pid_file != String::new() {
                        use std::io::prelude::*;
                        let mut file = ::std::fs::File::create(pid_file.clone()).expect("create pid file failed");
                        file.write_all(format!("{}", ::std::process::id()).as_ref()).expect("write error");
                    }
                    let mut list = command.get_all_args();
                    list.push("--FromForever".to_string());
                    loop {
                        reload_left_times-=1;
                        if reload_left_times <= 0 {
                            break
                        }
    
                        let now = ::std::time::SystemTime::now();
                        let mut child = if cfg!(target_os = "windows") {
                            ::std::process::Command::new(command.get_exec().unwrap())
                                    .args(list.clone())
                                    .stdin(::std::process::Stdio::inherit())
                                    .stdout(::std::process::Stdio::inherit())
                                    .spawn()
                                    .expect("failed to execute process")
                        } else {
                            ::std::process::Command::new(command.get_exec().unwrap())
                                    .args(list.clone())
                                    .stdin(::std::process::Stdio::inherit())
                                    .stdout(::std::process::Stdio::inherit())
                                    .spawn()
                                    .expect("failed to execute process")
                        };


                        if cpid_file != String::new() {
                            use std::io::prelude::*;
                            let mut file = ::std::fs::File::create(cpid_file.clone()).expect("create pid file failed");
                            file.write_all(format!("{}", child.id()).as_ref()).expect("write error");
                        }
    
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

                        ::std::fs::remove_file(cpid_file.clone()).ok();
                    }

                    if pid_file != String::new() {
                        ::std::fs::remove_file(pid_file).ok();
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