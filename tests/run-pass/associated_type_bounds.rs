#![feature(associated_type_bounds)]

// https://github.com/rust-lang/rust/blob/7bd62a8f5a4d6d740677aea03c37771258529922/src/test/ui/associated-type-bounds/fn-where.rs
pub mod fn_where {
    use easy_ext::ext;

    use crate::fn_aux::*;

    #[ext(E1)]
    impl<T> T {
        fn where_bound<B>(beta: B) -> usize
        where
            B: Beta<Gamma: Alpha>,
        {
            desugared_bound(beta)
        }

        fn where_bound_region<B>(beta: B) -> usize
        where
            B: Beta<Gamma: 'static>,
        {
            desugared_bound_region(beta)
        }

        fn where_bound_multi<B>(beta: B) -> usize
        where
            B: Copy + Beta<Gamma: Alpha + 'static + Delta>,
        {
            desugared_bound_multi(beta)
        }

        fn where_bound_region_specific<'a, B>(gamma: &'a B::Gamma) -> usize
        where
            B: Beta<Gamma: 'a + Epsilon<'a>>,
        {
            desugared_bound_region_specific::<B>(gamma)
        }

        fn where_bound_region_forall<B>(beta: B) -> usize
        where
            B: Beta<Gamma: Copy + for<'a> Epsilon<'a>>,
        {
            desugared_bound_region_forall(beta)
        }

        fn where_bound_region_forall2<B>(beta: B) -> usize
        where
            B: Beta<Gamma: Copy + for<'a> Epsilon<'a, Zeta: Eta>>,
        {
            desugared_bound_region_forall2(beta)
        }

        fn where_contraint_region_forall<B>(beta: B) -> usize
        where
            for<'a> &'a B: Beta<Gamma: Alpha>,
        {
            desugared_contraint_region_forall(beta)
        }

        fn where_bound_nested<B>(beta: B) -> usize
        where
            B: Beta<Gamma: Copy + Alpha + Beta<Gamma: Delta>>,
        {
            desugared_bound_nested(beta)
        }
    }
}

// https://github.com/rust-lang/rust/blob/7bd62a8f5a4d6d740677aea03c37771258529922/src/test/ui/associated-type-bounds/auxiliary/fn-aux.rs
mod fn_aux {
    // Traits:

    pub trait Alpha {
        fn alpha(self) -> usize;
    }

    pub trait Beta {
        type Gamma;
        fn gamma(self) -> Self::Gamma;
    }

    pub trait Delta {
        fn delta(self) -> usize;
    }

    pub trait Epsilon<'a> {
        type Zeta;
        fn zeta(&'a self) -> Self::Zeta;

        fn epsilon(&'a self) -> usize;
    }

    pub trait Eta {
        fn eta(self) -> usize;
    }

    // Assertions:

    pub fn assert_alpha<T: Alpha>(x: T) -> usize {
        x.alpha()
    }
    pub fn assert_static<T: 'static>(_: T) -> usize {
        24
    }
    pub fn assert_delta<T: Delta>(x: T) -> usize {
        x.delta()
    }
    pub fn assert_epsilon_specific<'a, T: 'a + Epsilon<'a>>(x: &'a T) -> usize {
        x.epsilon()
    }
    pub fn assert_epsilon_forall<T: for<'a> Epsilon<'a>>() {}
    pub fn assert_forall_epsilon_zeta_satisfies_eta<T>(x: T) -> usize
    where
        T: for<'a> Epsilon<'a>,
        for<'a> <T as Epsilon<'a>>::Zeta: Eta,
    {
        x.epsilon() + x.zeta().eta()
    }

    // Implementations and types:

    #[derive(Copy, Clone)]
    pub struct BetaType;

    #[derive(Copy, Clone)]
    pub struct GammaType;

    #[derive(Copy, Clone)]
    pub struct ZetaType;

    impl Beta for BetaType {
        type Gamma = GammaType;
        fn gamma(self) -> Self::Gamma {
            GammaType
        }
    }

    impl<'a> Beta for &'a BetaType {
        type Gamma = GammaType;
        fn gamma(self) -> Self::Gamma {
            GammaType
        }
    }

    impl Beta for GammaType {
        type Gamma = Self;
        fn gamma(self) -> Self::Gamma {
            self
        }
    }

    impl Alpha for GammaType {
        fn alpha(self) -> usize {
            42
        }
    }

    impl Delta for GammaType {
        fn delta(self) -> usize {
            1337
        }
    }

    impl<'a> Epsilon<'a> for GammaType {
        type Zeta = ZetaType;
        fn zeta(&'a self) -> Self::Zeta {
            ZetaType
        }

        fn epsilon(&'a self) -> usize {
            7331
        }
    }

    impl Eta for ZetaType {
        fn eta(self) -> usize {
            7
        }
    }

    // Desugared forms to check against:

    pub fn desugared_bound<B>(beta: B) -> usize
    where
        B: Beta,
        B::Gamma: Alpha,
    {
        let gamma: B::Gamma = beta.gamma();
        assert_alpha::<B::Gamma>(gamma)
    }

    pub fn desugared_bound_region<B>(beta: B) -> usize
    where
        B: Beta,
        B::Gamma: 'static,
    {
        assert_static::<B::Gamma>(beta.gamma())
    }

    pub fn desugared_bound_multi<B>(beta: B) -> usize
    where
        B: Copy + Beta,
        B::Gamma: Alpha + 'static + Delta,
    {
        assert_alpha::<B::Gamma>(beta.gamma())
            + assert_static::<B::Gamma>(beta.gamma())
            + assert_delta::<B::Gamma>(beta.gamma())
    }

    pub fn desugared_bound_region_specific<'a, B>(gamma: &'a B::Gamma) -> usize
    where
        B: Beta,
        B::Gamma: 'a + Epsilon<'a>,
    {
        assert_epsilon_specific::<B::Gamma>(gamma)
    }

    pub fn desugared_bound_region_forall<B>(beta: B) -> usize
    where
        B: Beta,
        B::Gamma: Copy + for<'a> Epsilon<'a>,
    {
        assert_epsilon_forall::<B::Gamma>();
        let g1: B::Gamma = beta.gamma();
        let g2: B::Gamma = g1;
        assert_epsilon_specific::<B::Gamma>(&g1) + assert_epsilon_specific::<B::Gamma>(&g2)
    }

    pub fn desugared_bound_region_forall2<B>(beta: B) -> usize
    where
        B: Beta,
        B::Gamma: Copy + for<'a> Epsilon<'a>,
        for<'a> <B::Gamma as Epsilon<'a>>::Zeta: Eta,
    {
        let gamma = beta.gamma();
        assert_forall_epsilon_zeta_satisfies_eta::<B::Gamma>(gamma)
    }

    pub fn desugared_contraint_region_forall<B>(beta: B) -> usize
    where
        for<'a> &'a B: Beta,
        for<'a> <&'a B as Beta>::Gamma: Alpha,
    {
        let g1 = beta.gamma();
        let g2 = beta.gamma();
        assert_alpha(g1) + assert_alpha(g2)
    }

    pub fn desugared_bound_nested<B>(beta: B) -> usize
    where
        B: Beta,
        B::Gamma: Copy + Alpha + Beta,
        <B::Gamma as Beta>::Gamma: Delta,
    {
        let go = beta.gamma();
        let gi = go.gamma();
        go.alpha() + gi.delta()
    }

    pub fn desugared() {
        let beta = BetaType;
        let gamma = beta.gamma();

        assert_eq!(42, desugared_bound(beta));
        assert_eq!(24, desugared_bound_region(beta));
        assert_eq!(42 + 24 + 1337, desugared_bound_multi(beta));
        assert_eq!(7331, desugared_bound_region_specific::<BetaType>(&gamma));
        assert_eq!(7331 * 2, desugared_bound_region_forall(beta));
        assert_eq!(42 + 1337, desugared_bound_nested(beta));
    }
}

fn main() {}
