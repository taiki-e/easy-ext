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

    #[rustfmt::skip]
    #[ext(ExtraArg,)] //~ ERROR unexpected token: `,`
    impl str {}

    #[ext(pub OldVisSyntax1)] //~ ERROR use `pub impl` instead
    impl str {}

    #[ext(pub(crate) OldVisSyntax2)] //~ ERROR use `pub(crate) impl` instead
    impl str {}
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
}

fn main() {}
