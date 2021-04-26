// See also `maybe` test in test.rs

use easy_ext::ext;

#[ext(E1)]
impl<T: ?Sized> T // Ok
{
    fn f(&self) {}
}

#[ext(E2)]
impl<T> T
where
    T: ?Sized, // Ok
{
    fn f(&self) {}
}

#[ext(E3)]
impl<T> T
where
    Self: ?Sized, //~ ERROR `?Trait` bounds are only permitted at the point where a type parameter is declared
{
    fn f(&self) {}
}

// The following is a case where #[ext] is not used. The behavior should match in both cases.

trait T1
where
    Self: ?Sized,
{
    //~^^ ERROR `?Trait` bounds are only permitted at the point where a type parameter is declared
    fn f(&self);
}

trait T2 {
    fn f(&self);
}
impl<T> T2 for T
where
    Self: ?Sized, //~ ERROR `?Trait` bounds are only permitted at the point where a type parameter is declared
{
    fn f(&self) {}
}

trait T3 {
    fn f(&self);
}
impl<T: ?Sized> T3 for T // Ok
{
    fn f(&self) {}
}

trait T4 {
    fn f(&self);
}
impl<T> T4 for T
where
    T: ?Sized, // Ok
{
    fn f(&self) {}
}

trait T5 {
    fn f(&self);
}
impl<T> T5 for T {
    fn f(&self)
    where
        T: ?Sized, //~ ERROR `?Trait` bounds are only permitted at the point where a type parameter is declared
    {
    }
}

fn main() {}
