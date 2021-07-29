#![feature(type_alias_impl_trait)]

use easy_ext::ext;

#[ext(E1)]
impl<T, I> I
where
    I: Iterator<Item = T>,
{
    type Assoc = impl Iterator<Item = T>;
    fn assoc(self) -> Self::Assoc {
        self
    }
}

fn main() {}
