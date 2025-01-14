// SPDX-License-Identifier: Apache-2.0 OR MIT

#![allow(incomplete_features)]
#![feature(specialization)]

// See also run-pass/min_specialization.rs.

pub mod default_impl {
    // I don't feel `default impl` is the good feature to combine with ext trait, but test anyway.

    // https://github.com/rust-lang/rust/blob/1.84.0/tests/ui/specialization/defaultimpl/auxiliary/go_trait.rs
    pub mod go_trait {
        use easy_ext::ext;

        pub trait Go {
            fn go(&self, arg: isize);
        }

        pub fn go<G: Go>(this: &G, arg: isize) {
            this.go(arg)
        }

        pub fn go_mut<G: GoMut>(this: &mut G, arg: isize) {
            this.go_mut(arg)
        }

        pub fn go_once<G: GoOnce>(this: G, arg: isize) {
            this.go_once(arg)
        }

        #[ext(GoMut)]
        pub default impl<G> G
        where
            G: Go,
        {
            fn go_mut(&mut self, arg: isize) {
                go(&*self, arg)
            }
        }

        #[ext(GoOnce)]
        pub default impl<G> G
        where
            G: GoMut,
        {
            fn go_once(mut self, arg: isize) {
                go_mut(&mut self, arg)
            }
        }
    }
}

fn main() {}
