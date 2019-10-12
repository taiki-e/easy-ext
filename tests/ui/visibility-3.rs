mod foo {
    use easy_ext::ext;

    #[ext(StrExt)]
    impl str {
        fn bar(&self, pat: &str) -> String {
            self.replace(pat, "_")
        }
    }
}

fn main() {
    use foo::StrExt; //~ ERROR trait `StrExt` is private [E0603]
}
