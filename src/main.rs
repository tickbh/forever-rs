
// use std::process::{Command, Stdio};
// use std::env;
// use std::thread::sleep;
// use std::time;
// use std::fs::File;
// use std::io::prelude::*;

#[forever_rs::main(monitor_file="Cargo.lock", max_times=3)]
fn main() {
    println!("Hello, world!");

        
    // let mut file = File::create("foo.txt").expect("aaaa");
    // file.write_all(b"Hello, world!").expect("aaaa");

    // sleep(time::Duration::from_millis(100000));
    // println!("fuck!!!!!!!!");
    // return
}

