
use std::process::{Command, Stdio};
use std::env;
use std::thread::sleep;
use std::{thread, time};
use std::fs::File;

#[forever_rs::main]
fn main() {
    println!("Hello, world!");

    let mut file = File::create("foo.txt").expect("aaaa");
    file.write_all(b"Hello, world!").expect("aaaa");

    loop {
        sleep(time::Duration::from_millis(10000));
        println!("fuck!!!!!!!!");
    }

}

