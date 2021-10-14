#![feature(generic_associated_types)]

// https://github.com/rust-lang/rust/blob/7bd62a8f5a4d6d740677aea03c37771258529922/src/test/ui/generic-associated-types/collections.rs
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
        type Iter<'iter>
        where
            T: 'iter,
        = std::slice::Iter<'iter, T>;
        type Family = VecFamily;
        type Sibling<U> = <<Self as Collection<T>>::Family as CollectionFamily>::Member<U>;

        fn empty() -> Self {
            Vec::new()
        }

        fn add(&mut self, value: T) {
            self.push(value)
        }

        fn iterate<'iter>(&'iter self) -> Self::Iter<'iter> {
            self.iter()
        }
    }
}

fn main() {}
