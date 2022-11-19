
// use std::process::{Command, Stdio};
// use std::env;
// use std::thread::sleep;
// use std::time;
// use std::fs::File;
// use std::io::prelude::*;

#[forever_rs::main(monitor_file="Cargo.lock", max_times=6, daemon=false, pid_file="forever.pid")]
fn main() {
    println!("Hello, world!");

    panic!("fuck");
    // loop {
    //     ::std::thread::sleep(::std::time::Duration::from_millis(1000));
    // }
}

