//! Helper module which defines `FnArgs`
//! to make function calling easier.

use any::Any;

pub trait FunArgs<'a> {
    fn into_vec(self) -> Vec<&'a mut Any>;
}

macro_rules! impl_args {
    ($($p:ident),*) => {
        impl<'a, $($p),*> FunArgs<'a> for ($(&'a mut $p,)*)
        where
            $($p: Any + Clone),*
        {
            fn into_vec(self) -> Vec<&'a mut Any> {
                let ($($p,)*) = self;

                let mut v = Vec::new();
                $(v.push($p as &mut Any);)*

                v
            }
        }

        impl_args!(@pop $($p),*);
    };
    (@pop) => {
    };
    (@pop $head:ident $(, $tail:ident)*) => {
        impl_args!($($tail),*);
    };
}

#[cfg_attr(rustfmt, rustfmt_skip)]
impl_args!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S);
