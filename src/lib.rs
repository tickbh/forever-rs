#[allow(unused_extern_crates)]
extern crate proc_macro;

mod ok;

#[proc_macro_attribute]
pub fn main(args: TokenStream, input: TokenStream) -> TokenStream {
    ok::main(args, input)
}

use proc_macro::TokenStream;
