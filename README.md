## forever-rs
rust守护进程, 轻松实现守护

## example 
监控文件改变就自动重启, 或者子进程意外退出重启
```
#[forever_rs::main(monitor_file="Cargo.lock", max_times=6)]
fn main() {
    println!("Hello, world!");
}
```