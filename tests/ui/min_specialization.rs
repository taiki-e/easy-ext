#![feature(min_specialization)]

// See also run-pass/min_specialization.rs.

// https://github.com/rust-lang/rust/blob/7bd62a8f5a4d6d740677aea03c37771258529922/src/test/ui/specialization/min_specialization/spec-iter.rs
pub mod spec_iter {
    use easy_ext::ext;

    #[ext(SpecFromIter)]
    impl<'a, T: 'a, I: Iterator<Item = &'a T>> I {
        default fn f(&self) {}
    }

    // This will not compile because all of the generics in the default
    // implementation are used in the generics of the trait.
    // To fix compile error, you will need to add `'a` to `SpecFromIter`.
    // See also spec_iter module in run-pass/min_specialization.rs.
    impl<'a, T> SpecFromIter<T> for std::slice::Iter<'a, T> {
        fn f(&self) {}
    }
}

fn main() {}
