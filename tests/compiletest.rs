#![warn(rust_2018_idioms, single_use_lifetimes)]

use std::env;

#[rustversion::attr(not(nightly), ignore)]
#[test]
fn ui() {
    if !env::var("CI").is_ok() {
        env::set_var("TRYBUILD", "overwrite");
    }

    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}
