#![warn(unsafe_code)]
#![warn(rust_2018_idioms)]

use easy_ext::ext;

#[test]
fn test_simple() {
    #[ext(StrExt)]
    impl str {
        fn foo(&self, pat: &str) -> String {
            self.replace(pat, "_")
        }
    }

    assert_eq!("--".foo("-").as_str(), "__");
}

#[test]
fn test_params() {
    #[ext(ResultExt)]
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
fn test_lifetime() {
    #[ext(OptionExt)]
    impl<'a, T> &'a mut Option<T> {
        fn into_ref(self) -> Option<&'a T> {
            self.as_ref()
        }
    }

    let _: Option<&u8> = (&mut Some(1)).into_ref();
}

#[test]
fn test_generics() {
    #[ext(IterExt)]
    impl<I: IntoIterator> I {
        fn _next(self) -> Option<I::Item> {
            self.into_iter().next()
        }
    }

    assert_eq!(vec![1, 2, 3]._next(), Some(1u8));
}

/*

## Visibility

* The generated extension trait inherits the visibility of the item in the original `impl`.

* The visibility of all the items in the original `impl` must be identical.

*/

mod bar {
    use easy_ext::ext;

    #[ext(StrExt)]
    impl str {
        pub const FOO: &'static str = "_";
        // const FOO2: &'static str = "_"; // ERROR visibility mismatch

        pub fn foo(&self, pat: &str) -> String {
            self.replace(pat, Self::FOO)
        }
    }

    pub(super) mod baz {
        use easy_ext::ext;

        #[ext(StrExt)]
        impl str {
            pub(super) fn bar(&self, pat: &str) -> String {
                self.replace(pat, "_")
            }
        }

        #[ext(StrExt3)]
        impl str {
            // pub type Owned = String;

            pub fn baz(&self, pat: &str) -> String {
                self.replace(pat, "_")
            }

            pub fn baz2(&self, pat: &str) -> String {
                self.replace(pat, "-")
            }

            // ERROR visibility mismatch
            // fn baz2(&self, pat: &str) -> String {
            //     self.replace(pat, "_")
            // }
        }
    }
}

#[test]
fn test_vis() {
    // use bar::baz::StrExt as StrExt2; // ERROR trait `StrExt` is private
    use self::bar::{baz::StrExt3, StrExt};

    assert_eq!("--".foo("-").as_str(), "__");
    assert_eq!("--".baz("-").as_str(), "__");
}
