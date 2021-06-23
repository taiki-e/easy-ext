#![feature(const_generics_defaults)]
#![feature(custom_inner_attributes)]
#![rustfmt::skip]

// https://github.com/rust-lang/rust/tree/0ced530534de1a77504b6229e6303d37a12282ee/src/test/ui/const-generics/defaults

use easy_ext::ext;

#[ext(Ext)]
impl<const N: usize = 3> () {}
impl Ext for u8 {}

// The code above is equivalent to the code below.

trait Trait<const N: usize = 3> {}
impl<const N: usize> Trait<N> for () {}
impl Trait for u8 {}

// https://github.com/rust-lang/rust/blob/716394d6581b60c75cfdd88b8e5b876f2db88b62/src/test/ui/const-generics/defaults/const-param-as-default-value.rs
#[ext(Ext2)]
impl<const N: usize, const M: usize = N> () {}

fn main() {}
