#![feature(generic_associated_types)]

// https://github.com/rust-lang/rust/blob/1.62.0/src/test/ui/generic-associated-types/collections.rs
pub mod collections {
    use easy_ext::ext;

    trait CollectionFamily {
        type Member<T>: Collection<T, Family = Self>;
    }

    struct VecFamily;

    impl CollectionFamily for VecFamily {
        type Member<T> = Vec<T>;
    }

    #[ext(Collection)]
    impl<T> Vec<T> {
        // TODO: handle where clause in GAT: https://github.com/rust-lang/rust/pull/90076
        // type Iter<'iter> = std::slice::Iter<'iter, T>
        // where
        //     T: 'iter,
        //     Self: 'iter;
        type Family = VecFamily;
        type Sibling<U> = <<Self as Collection<T>>::Family as CollectionFamily>::Member<U>;

        fn empty() -> Self {
            Vec::new()
        }

        fn add(&mut self, value: T) {
            self.push(value)
        }

        // TODO: handle where clause in GAT: https://github.com/rust-lang/rust/pull/90076
        // fn iterate<'iter>(&'iter self) -> Self::Iter<'iter> {
        //     self.iter()
        // }
    }
}

fn main() {}
