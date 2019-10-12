use easy_ext::ext;

#[ext(StrExt)]
impl str {
    pub const FOO: &'static str = "_";
    const Bar: &'static str = "_"; //~ ERROR All items must have a visibility of `pub`

    pub fn baz(&self) -> String {}
}

fn main() {}
