#[allow(unused_extern_crates)]
extern crate proc_macro;

mod entry;
// mod forever;
// use forever::ForeverConfig;

#[proc_macro_attribute]
pub fn main(args: TokenStream, input: TokenStream) -> TokenStream {
    entry::main(args, input)
}

// pub fn start_forever() {
//     // entry::main(args, input)
// }

// #[proc_macro_attribute]
// pub use forever::ForeverConfig;

use proc_macro::TokenStream;
