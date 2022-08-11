#![warn(rust_2018_idioms, single_use_lifetimes)]
#![allow(clippy::missing_safety_doc)]

use async_trait::async_trait;
use easy_ext::ext;

#[test]
fn simple() {
    #[ext]
    impl str {
        fn foo(&self, pat: &str) -> String {
            self.replace(pat, "_")
        }
    }

    assert_eq!("--".foo("-"), "__");
}

#[test]
fn params() {
    #[ext]
    impl<T, E> Result<T, E> {
        fn err_into<U>(self) -> Result<T, U>
        where
            E: Into<U>,
        {
            self.map_err(Into::into)
        }
    }

    let err: Result<(), _> = Err(1_u32);
    assert_eq!(err.err_into::<u64>().unwrap_err(), 1_u64);
}

#[test]
fn lifetime() {
    #[ext(OptionExt)]
    impl<'a, T> &'a mut Option<T> {
        fn into_ref(self) -> Option<&'a T> {
            self.as_ref()
        }
    }

    let _: Option<&u8> = Some(1).into_ref();
}

mod bar {
    use easy_ext::ext;

    // assoc-item-level visibility + named
    #[ext(E1)]
    impl str {
        pub const FOO1: &'static str = "_";

        pub fn foo1(&self, pat: &str) -> String {
            self.replace(pat, Self::FOO1)
        }
    }

    // assoc-item-level visibility + unnamed
    #[ext]
    impl str {
        pub const FOO2: &'static str = "_";

        pub fn foo2(&self, pat: &str) -> String {
            self.replace(pat, Self::FOO2)
        }
    }

    // impl-level visibility + named
    #[ext(E2)]
    pub impl str {
        const FOO3: &'static str = "_";

        fn foo3(&self, pat: &str) -> String {
            self.replace(pat, Self::FOO3)
        }
    }

    // impl-level visibility + unnamed
    #[ext]
    pub impl str {
        const FOO4: &'static str = "_";

        fn foo4(&self, pat: &str) -> String {
            self.replace(pat, Self::FOO4)
        }
    }

    pub(super) mod baz {
        use easy_ext::ext;

        #[ext(E4)]
        impl str {
            pub(super) fn bar(&self, pat: &str) -> String {
                self.replace(pat, "_")
            }
        }

        #[ext(E5)]
        impl str {
            pub fn baz(&self, pat: &str) -> String {
                self.replace(pat, "_")
            }

            pub fn baz2(&self, pat: &str) -> String {
                self.replace(pat, "-")
            }
        }

        #[ext(E6)]
        pub(super) impl str {
            fn bar2(&self, pat: &str) -> String {
                self.replace(pat, "_")
            }
        }

        #[ext(E7)]
        pub(crate) impl str {
            fn baz3(&self, pat: &str) -> String {
                self.replace(pat, "_")
            }

            fn baz4(&self, pat: &str) -> String {
                self.replace(pat, "-")
            }
        }
    }
}

#[test]
fn visibility() {
    use self::bar::{
        baz::{E5, E7},
        E1, E2,
    };

    assert_eq!("..".foo1("."), "__");
    assert_eq!("..".foo3("."), "__");
    assert_eq!("..".baz("."), "__");
    assert_eq!("..".baz2("."), "--");
    assert_eq!("..".baz3("."), "__");
    assert_eq!("..".baz4("."), "--");
}

#[test]
fn generics() {
    #[ext(IterExt)]
    impl<I: IntoIterator> I {
        fn _next(self) -> Option<I::Item> {
            self.into_iter().next()
        }
    }

    assert_eq!(vec![1, 2, 3]._next(), Some(1_u8));
}

#[test]
fn trait_generics() {
    #[derive(Debug, PartialEq, Eq)]
    struct A {}

    impl Iterator for A {
        type Item = ();
        fn next(&mut self) -> Option<Self::Item> {
            None
        }
    }

    #[ext(ConstInit)]
    impl A {
        const INIT1: Self = Self {};
        const INIT2: A = A {};
    }

    #[ext(Ext1)]
    impl<I: Iterator + ConstInit> I {
        const CONST1: Self = Self::INIT1;
        const CONST2: I = I::INIT1;
        type Item2 = Self::Item;
        type Item3 = I::Item;
        fn method1(mut self) -> Option<Self::Item> {
            self.next()
        }
        fn method2(mut self) -> Option<I::Item> {
            self.next()
        }
        fn method3(mut self) -> Option<Self::Item2> {
            self.next()
        }
        fn method4(mut self) -> Option<<I as Ext1>::Item3> {
            self.next()
        }
    }

    fn a<T: Ext1 + Eq + std::fmt::Debug>(mut x: T) {
        let y = T::CONST1;
        let _ = T::CONST2;
        assert_eq!(x, y);
        assert!(x.next().is_none());
    }

    assert_eq!(A {}.method1(), None);
    assert_eq!(A {}.method2(), None);

    a(A::INIT1);
    a(A::INIT2);

    #[ext(Ext2)]
    impl<I: Iterator + ConstInit> I {
        const CONST3: I = {
            #[allow(dead_code)]
            fn a<I>() {}
            I::INIT1
        };
        type Item4 = I::Item;
        fn method5(self, _: I::Item) -> (Option<I::Item>, <I as Ext2>::Item4) {
            #[allow(dead_code)]
            fn a<I>() {}
            unimplemented!()
        }
    }
}

#[test]
fn type_parameter_defaults() {
    #[ext(Ext)]
    impl<T = ()> () {}
    impl Ext for u8 {}

    // The code above is equivalent to the code below.

    trait Trait<T = ()> {}
    impl<T> Trait<T> for () {}
    impl Trait for u8 {}
}

// See also ui/maybe.rs
#[test]
fn maybe() {
    #[ext]
    impl<T: ?Sized> T {
        fn f(&self) {}
    }

    #[ext]
    impl<T> T
    where
        T: ?Sized,
    {
        fn f(&self) {}
    }

    #[ext]
    impl<T: Send + ?Sized + Sync> T {
        fn f(&self) {}
    }

    #[ext]
    impl<T> T
    where
        T: Send + ?Sized + Sync,
    {
        fn f(&self) {}
    }

    #[ext]
    impl<T> T
    where
        T: Iterator,
        T: ?Sized,
        T: Default,
    {
        fn f(&self) {}
    }
}

#[test]
fn inline() {
    #[ext]
    impl str {
        #[inline]
        fn auto(&self) {}
        #[inline(always)]
        fn always(&self) {}
        #[inline(never)]
        fn never(&self) {}
    }
}

#[test]
fn assoc_ty() {
    #[ext(StrExt)]
    impl str {
        type Assoc = String;

        fn owned(&self) -> Self::Assoc {
            self.to_string()
        }
    }

    let s: <str as StrExt>::Assoc = "?".owned();
    assert_eq!(s, "?");

    #[ext(TryIterator)]
    impl<I: Iterator<Item = Result<T, E>>, T, E> I {
        type Ok = T;
        type Error = E;

        fn try_next(&mut self) -> Result<Option<Self::Ok>, Self::Error> {
            self.next().transpose()
        }
    }

    let mut iter = vec![Ok(1), Err(1)].into_iter();
    assert_eq!(iter.try_next(), Ok(Some(1)));
    assert_eq!(iter.try_next(), Err(1));
    assert_eq!(iter.try_next(), Ok(None));
}

#[test]
fn syntax() {
    #[ext(E1)]
    unsafe impl str {
        fn normal(&self) {}
        unsafe fn unsafety(&self) {}
        extern "C" fn abi1() {}
        extern "C" fn abi2() {}
        unsafe extern "C" fn unsafe_abi1() {}
        unsafe extern "C" fn unsafe_abi2() {}
    }

    "a".normal();
    unsafe { "?".unsafety() };
    str::abi1();
    unsafe { str::unsafe_abi1() };

    struct S {}
    unsafe impl E1 for S {
        fn normal(&self) {}
        unsafe fn unsafety(&self) {}
        extern "C" fn abi1() {}
        extern "C" fn abi2() {}
        unsafe extern "C" fn unsafe_abi1() {}
        unsafe extern "C" fn unsafe_abi2() {}
    }

    #[ext(E2)]
    #[async_trait]
    impl str {
        async fn asyncness(&self) {}
        async unsafe fn unsafe_asyncness(&self) {}
    }

    let _ = async {
        "a".asyncness().await;
        unsafe { "b".unsafe_asyncness().await }
    };

    #[async_trait]
    impl E2 for S {
        async fn asyncness(&self) {}
        async unsafe fn unsafe_asyncness(&self) {}
    }
}

// test for angle bracket
#[test]
fn angle_bracket() {
    #[ext]
    impl fn() -> () {
        const FUNC: fn(&u8) -> bool = u8::is_ascii as fn(&u8) -> bool;
        type Func = fn() -> fn() -> ();
        fn func() -> fn() -> fn() -> () {
            || || {}
        }
    }

    #[ext(E1)]
    impl<T> T
    where
        Self::Assoc3: Sized,
        T::Assoc3: Sized,
        Self: E2,
        T: E2,
    {
        const ASSOC1: <Self>::Assoc1 = <Self>::assoc1;
        type Assoc1 = fn() -> <Self>::Assoc2;
        type Assoc2 = ();
        fn assoc1() -> <Self>::Assoc2
        where
            <Self as E1>::Assoc1: Sized,
            <T as E1>::Assoc1: Sized,
            Self::Assoc1: Sized,
            T::Assoc3: Sized,
            Self: E2,
            T: E2,
        {
        }
    }

    struct A {}
    #[ext(E2)]
    impl A {
        const ASSOC1: <Self>::Assoc3 = <Self>::assoc2;
        type Assoc3 = fn() -> <Self>::Assoc4;
        type Assoc4 = ();
        fn assoc2() -> <Self>::Assoc4
        where
            <Self as E2>::Assoc3: Sized,
            Self::Assoc3: Sized,
        {
        }
    }

    #[ext]
    impl<T: Fn() -> fn() -> T, E> Result<T, E>
    where
        E: FnOnce() -> Result<T, E>,
        &'static dyn Fn() -> T: Fn() -> T + 'static,
        fn() -> fn() -> T: Fn() -> fn() -> T,
    {
        fn where_clause<U: Fn() -> fn() -> T, F>(self, _f: F) -> Self
        where
            F: FnOnce() -> Result<U, E>,
            &'static dyn Fn() -> T: Fn() -> T + 'static,
            fn() -> fn() -> T: Fn() -> fn() -> T,
        {
            unimplemented!()
        }
    }
}

#[test]
fn min_const_generics() {
    struct S1<T, const CAP: usize>([T; CAP]);
    #[ext(E1)]
    impl<T, const CAP: usize> S1<T, CAP> {
        const CAPACITY: usize = CAP;
        fn f<const C: usize>() -> S1<T, C> {
            unimplemented!()
        }
    }

    struct S2<const CAP: usize>;
    impl<const CAP: usize> E1<(), { CAP }> for S2<{ CAP }> {
        const CAPACITY: usize = CAP;
        fn f<const C: usize>() -> S1<(), { C }> {
            S1([(); C])
        }
    }

    let _: [(); 2] = <S2<3>>::f::<2>().0;

    struct S3<T, const CAP: char>(T);

    #[ext(E2)]
    impl str {
        fn method1(&self) -> S1<Option<fn() -> ()>, 1> {
            S1([Some(|| {})])
        }
        #[allow(unused_braces)]
        fn method2(&self) -> S1<Option<fn() -> ()>, { 1 }> {
            S1([Some(|| {})])
        }
        fn method3(&self) -> S3<fn() -> (), 'a'> {
            S3(|| {})
        }
        #[allow(unused_braces)]
        fn method4(&self) -> S3<fn() -> (), { 'a' }> {
            S3(|| {})
        }
    }
}

#[test]
fn const_generics_defaults() {
    // https://github.com/rust-lang/rust/tree/1.63.0/src/test/ui/const-generics/defaults

    #[ext(Ext)]
    impl<const N: usize = 3> () {}
    impl Ext for u8 {}

    // The code above is equivalent to the code below.

    trait Trait<const N: usize = 3> {}
    impl<const N: usize> Trait<N> for () {}
    impl Trait for u8 {}

    // https://github.com/rust-lang/rust/blob/1.63.0/src/test/ui/const-generics/defaults/const-param-as-default-value.rs
    #[ext(Ext2)]
    impl<const N: usize, const M: usize = N> () {}
}

#[test]
fn macros() {
    macro_rules! m {
        (
            $impl:ident $path:path [$($generics:tt)*] $where:ident {$(
                [$vis:vis, $($fn_sig:ident)*]
            )*}
        ) => {
            $(
                #[ext]
                $impl<T, E> Result<T, E> {
                    $vis $($fn_sig)* <U>(self) -> Result<T, U>
                    where
                        E: Into<U>,
                    {
                        unimplemented!()
                    }
                }
            )*
        };
    }

    m!(impl Result [T,E] where {
        [, fn a]
        [pub, fn b]
        [pub, unsafe fn c]
        [pub(crate), fn d]
    });
}

// https://github.com/taiki-e/easy-ext/issues/36
#[test]
fn where_clause() {
    pub trait Trait<T> {}

    #[rustfmt::skip]
    #[ext]
    pub impl<T> Vec<T>
    where
        Self: Trait<Vec<T>>
    {
    }
}

pub mod visibility {
    use easy_ext::ext;

    pub struct Pub;
    #[ext]
    impl str {
        pub const ASSOC: u8 = 1;
        pub type Assoc = u8;
        pub fn assoc() {}
    }

    pub struct PubCrate;
    #[ext]
    impl PubCrate {
        pub(crate) const ASSOC: u8 = 1;
        pub(crate) type Assoc = u8;
        pub(crate) fn assoc() {}
    }

    pub struct PubSelf;
    #[ext]
    impl PubSelf {
        pub(self) const ASSOC: u8 = 1;
        pub(self) type Assoc = u8;
        pub(self) fn assoc() {}
    }

    pub mod m {
        use easy_ext::ext;

        pub struct PubSuper;
        #[ext]
        impl PubSuper {
            pub(super) const ASSOC: u8 = 1;
            pub(super) type Assoc = u8;
            pub(super) fn assoc() {}
        }

        pub struct PubIn;
        #[ext]
        impl PubIn {
            pub(in super::m) const ASSOC: u8 = 1;
            pub(in super::m) type Assoc = u8;
            pub(in super::m) fn assoc() {}
        }
    }
}
