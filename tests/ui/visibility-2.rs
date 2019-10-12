use easy_ext::ext;

#[ext(StrExt)]
impl str {
    fn foo(&self) -> String {}

    pub fn bar(&self) -> String {} //~ ERROR All items must have inherited visibility
}

fn main() {}
