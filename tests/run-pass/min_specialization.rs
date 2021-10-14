#![feature(min_specialization)]

// See also run-pass/specialization.rs.

// https://github.com/rust-lang/rust/blob/7bd62a8f5a4d6d740677aea03c37771258529922/src/test/ui/specialization/min_specialization/implcit-well-formed-bounds.rs
pub mod implcit_well_formed_bounds {
    use easy_ext::ext;

    struct OrdOnly<T: Ord>(T);

    #[ext(SpecTrait)]
    impl<T, U> T {
        default fn f() {}
    }

    impl<T: Ord> SpecTrait<()> for OrdOnly<T> {
        fn f() {}
    }

    impl<T: Ord> SpecTrait<OrdOnly<T>> for () {
        fn f() {}
    }

    impl<T: Ord, U: Ord, V: Ord> SpecTrait<(OrdOnly<T>, OrdOnly<U>)> for &[OrdOnly<V>] {
        fn f() {}
    }
}

// https://github.com/rust-lang/rust/blob/7bd62a8f5a4d6d740677aea03c37771258529922/src/test/ui/specialization/min_specialization/spec-iter.rs
pub mod spec_iter {
    use easy_ext::ext;

    #[ext(SpecFromIter)]
    impl<'a, T: 'a, I: Iterator<Item = &'a T>> I {
        default fn f(&self) {}
    }

    // See also spec_iter module in ui/min_specialization.rs.
    impl<'a, T> SpecFromIter<'a, T> for std::slice::Iter<'a, T> {
        fn f(&self) {}
    }
}

// https://github.com/rust-lang/rust/blob/7bd62a8f5a4d6d740677aea03c37771258529922/src/test/ui/specialization/min_specialization/spec-reference.rs
pub mod spec_reference {
    use easy_ext::ext;

    #[ext(MySpecTrait)]
    impl<T> T {
        default fn f() {}
    }

    impl<'a, T: ?Sized> MySpecTrait for &'a T {
        fn f() {}
    }
}

fn main() {}
