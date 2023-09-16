// SPDX-License-Identifier: Apache-2.0 OR MIT

#![feature(impl_trait_in_assoc_type)]

use easy_ext::ext;

#[ext(E1)]
impl<T, I> I
where
    I: Iterator<Item = T>,
{
    type Assoc = impl Iterator<Item = T>;
    fn assoc(self) -> Self::Assoc {
        self
    }
}

fn main() {}
