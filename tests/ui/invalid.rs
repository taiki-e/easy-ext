mod basic {
    use easy_ext::ext;

    #[ext(NoValueConst)]
    impl str {
        const ASSOC: u8; //~ ERROR expected `=`
    }
    #[ext(NoValueTy)]
    impl str {
        type Assoc; //~ ERROR expected `=`
    }
    #[ext(NoValueFn)]
    impl str {
        fn assoc(); //~ ERROR expected `{`
    }

    #[ext(Macro)]
    impl str {
        mac!(); //~ ERROR expected one of: `default`, `fn`, `const`, `type`
    }
}

mod visibility {
    use easy_ext::ext;

    #[ext(AssocLevel1)]
    impl str {
        pub const ASSOC1: u8 = 1;
        const ASSOC2: u8 = 2; //~ ERROR all associated items must have a visibility of `pub`
    }

    #[ext(AssocLevel2)]
    impl str {
        fn assoc1(&self) {}

        pub fn assoc2(&self) {} //~ ERROR all associated items must have inherited visibility
    }

    #[ext(AssocLevel3)]
    impl str {
        pub(crate) type Assoc1 = ();
        pub type Assoc2 = (); //~ ERROR all associated items must have a visibility of `pub(crate)`
    }

    #[ext(ImplLevel1)]
    pub impl str {
        fn assoc1(&self) {}

        pub fn assoc2(&self) {} //~ ERROR all associated items must have inherited visibility
    }

    #[ext(pub ImplLevel2)]
    impl str {
        fn assoc1(&self) {}

        pub fn assoc2(&self) {} //~ ERROR all associated items must have inherited visibility
    }

    #[ext(pub ImplLevel3)] //~ ERROR visibility can only be specified once
    pub impl str {
        fn assoc(&self) {}
    }
}

fn main() {}
