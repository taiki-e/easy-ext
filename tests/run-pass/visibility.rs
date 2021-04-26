#![feature(crate_visibility_modifier)]

use easy_ext::ext;

pub struct Pub;
#[ext]
impl str {
    pub const ASSOC: u8 = 1;
    pub type Assoc = u8;
    pub fn assoc() {}
}

pub struct Crate;
#[ext]
impl Crate {
    crate const ASSOC: u8 = 1;
    crate type Assoc = u8;
    crate fn assoc() {}
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

fn main() {}
