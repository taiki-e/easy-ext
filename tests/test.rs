use easy_ext::ext;

#[test]
fn test_example() {
    #[ext(StrExt)]
    impl str {
        fn foo(&self, pat: &str) -> String {
            self.replace(pat, "_")
        }
    }

    assert_eq!("--".foo("-").as_str(), "__");
}

/*

## Visibility

* The generated extension trait inherits the visibility of the original impl's item.

* The visibility of all the items in the original impl must be identical.

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
            pub fn baz(&self, pat: &str) -> String {
                self.replace(pat, "_")
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
    use bar::baz::StrExt3;
    use bar::StrExt;

    assert_eq!("--".foo("-").as_str(), "__");
    assert_eq!("--".baz("-").as_str(), "__");
}
