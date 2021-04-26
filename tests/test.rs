#![warn(rust_2018_idioms, single_use_lifetimes)]

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

    let _: Option<&u8> = (&mut Some(1)).into_ref();
}

/*

## Visibility

* The generated extension trait inherits the visibility of the item in the original `impl`.

* The visibility of all the items in the original `impl` must be identical.

*/

mod bar {
    use easy_ext::ext;

    // assoc-item-level visibility + named
    #[ext(StrExt)]
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
    #[ext(pub StrExt2)]
    impl str {
        const FOO3: &'static str = "_";

        fn foo3(&self, pat: &str) -> String {
            self.replace(pat, Self::FOO3)
        }
    }

    // impl-level visibility + unnamed
    #[ext(pub)]
    impl str {
        const FOO4: &'static str = "_";

        fn foo4(&self, pat: &str) -> String {
            self.replace(pat, Self::FOO4)
        }
    }

    pub(super) mod baz {
        use easy_ext::ext;

        #[ext(StrExt3)]
        impl str {
            pub(super) fn bar(&self, pat: &str) -> String {
                self.replace(pat, "_")
            }
        }

        #[ext(StrExt4)]
        impl str {
            pub fn baz(&self, pat: &str) -> String {
                self.replace(pat, "_")
            }

            pub fn baz2(&self, pat: &str) -> String {
                self.replace(pat, "-")
            }
        }

        #[ext(pub(super) StrExt5)]
        impl str {
            fn bar2(&self, pat: &str) -> String {
                self.replace(pat, "_")
            }
        }

        #[ext(pub StrExt6)]
        impl str {
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
        baz::{StrExt4, StrExt6},
        StrExt, StrExt2,
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

    #[ext(Ext)]
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
        fn method4(mut self) -> Option<<I as Ext>::Item3> {
            self.next()
        }
    }

    fn a<T: Ext + Eq + std::fmt::Debug>(mut x: T) {
        let y = T::CONST1;
        let _ = T::CONST2;
        assert_eq!(x, y);
        assert!(x.next().is_none());
    }

    assert_eq!(A {}.method1(), None);
    assert_eq!(A {}.method2(), None);

    a(A::INIT1);
    a(A::INIT2);
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
        #[inline]
        #[inline]
        fn auto2(&self) {}
        #[inline(always)]
        fn always(&self) {}
        #[inline(always)]
        #[inline(always)]
        fn always2(&self) {}
        #[inline(never)]
        fn never(&self) {}
        #[inline(never)]
        #[inline(never)]
        fn never2(&self) {}
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
        extern "C" fn abi() {}
        unsafe extern "C" fn unsafe_abi() {}
    }

    "a".normal();
    unsafe { "?".unsafety() };
    str::abi();
    unsafe { str::unsafe_abi() };

    struct S {}
    unsafe impl E1 for S {
        fn normal(&self) {}
        unsafe fn unsafety(&self) {}
        extern "C" fn abi() {}
        unsafe extern "C" fn unsafe_abi() {}
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

    #[ext(E3)]
    impl fn() -> () {
        const FUNC: fn(&str) -> () = str::normal as fn(&str) -> ();
        type Func = fn() -> fn() -> ();
        fn func() -> fn() -> fn() -> () {
            || || {}
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
            todo!()
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
