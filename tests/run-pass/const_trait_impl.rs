#![allow(incomplete_features)]
#![feature(const_trait_impl)]

// https://github.com/rust-lang/rust/issues/67792
// https://github.com/rust-lang/rust/blob/49920bc581743d6edb9f82fbff4cbafebc212619/src/test/ui/rfc-2632-const-trait-impl/call-const-trait-method-pass.rs

use easy_ext::ext;

#[ext(Ext)]
impl const i32 {
    fn plus(self, rhs: Self) -> Self {
        self + rhs
    }
}

pub const fn add_i32(a: i32, b: i32) -> i32 {
    a.plus(b)
}

const ADD_I32: i32 = 1i32.plus(2i32);

fn main() {}
