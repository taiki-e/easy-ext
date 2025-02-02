# easy-ext

[![crates.io](https://img.shields.io/crates/v/easy-ext?style=flat-square&logo=rust)](https://crates.io/crates/easy-ext)
[![docs.rs](https://img.shields.io/badge/docs.rs-easy--ext-blue?style=flat-square&logo=docs.rs)](https://docs.rs/easy-ext)
[![license](https://img.shields.io/badge/license-Apache--2.0_OR_MIT-blue?style=flat-square)](#license)
[![msrv](https://img.shields.io/badge/msrv-1.31-blue?style=flat-square&logo=rust)](https://www.rust-lang.org)
[![github actions](https://img.shields.io/github/actions/workflow/status/taiki-e/easy-ext/ci.yml?branch=main&style=flat-square&logo=github)](https://github.com/taiki-e/easy-ext/actions)

<!-- tidy:sync-markdown-to-rustdoc:start:src/lib.rs -->

A lightweight attribute macro for easily writing [extension trait pattern][rfc0445].

```toml
[dependencies]
easy-ext = "1"
```

## Examples

```rust
use easy_ext::ext;

#[ext(ResultExt)]
pub impl<T, E> Result<T, E> {
    fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>,
    {
        self.map_err(Into::into)
    }
}
```

Code like this will be generated:

```rust
pub trait ResultExt<T, E> {
    fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>,
    {
        self.map_err(Into::into)
    }
}
```

You can elide the trait name.

```rust
use easy_ext::ext;

#[ext]
impl<T, E> Result<T, E> {
    fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>,
    {
        self.map_err(Into::into)
    }
}
```

Note that in this case, `#[ext]` assigns a random name, so you cannot
import/export the generated trait.

### Visibility

There are two ways to specify visibility.

#### Impl-level visibility

The first way is to specify visibility at the impl level. For example:

```rust
use easy_ext::ext;

// unnamed
#[ext]
pub impl str {
    fn foo(&self) {}
}

// named
#[ext(StrExt)]
pub impl str {
    fn bar(&self) {}
}
```

#### Associated-item-level visibility

Another way is to specify visibility at the associated item level.

For example, if the method is `pub` then the trait will also be `pub`:

```rust
use easy_ext::ext;

#[ext(ResultExt)] // generate `pub trait ResultExt`
impl<T, E> Result<T, E> {
    pub fn err_into<U>(self) -> Result<T, U>
    where
        E: Into<U>,
    {
        self.map_err(Into::into)
    }
}
```

This is useful when migrate from an inherent impl to an extension trait.

Note that the visibility of all the associated items in the `impl` must be identical.

Note that you cannot specify impl-level visibility and associated-item-level visibility at the same time.

### [Supertraits](https://doc.rust-lang.org/reference/items/traits.html#supertraits)

If you want the extension trait to be a subtrait of another trait,
add `Self: SubTrait` bound to the `where` clause.

```rust
use easy_ext::ext;

#[ext(Ext)]
impl<T> T
where
    Self: Default,
{
    fn method(&self) {}
}
```

### Supported items

#### [Associated functions (methods)](https://doc.rust-lang.org/reference/items/associated-items.html#associated-functions-and-methods)

```rust
use easy_ext::ext;

#[ext]
impl<T> T {
    fn method(&self) {}
}
```

#### [Associated constants](https://doc.rust-lang.org/reference/items/associated-items.html#associated-constants)

```rust
use easy_ext::ext;

#[ext]
impl<T> T {
    const MSG: &'static str = "Hello!";
}
```

#### [Associated types](https://doc.rust-lang.org/reference/items/associated-items.html#associated-types)

```rust
use easy_ext::ext;

#[ext]
impl str {
    type Owned = String;

    fn method(&self) -> Self::Owned {
        self.to_owned()
    }
}
```

[rfc0445]: https://rust-lang.github.io/rfcs/0445-extension-trait-conventions.html

<!-- tidy:sync-markdown-to-rustdoc:end -->

## License

Licensed under either of [Apache License, Version 2.0](LICENSE-APACHE) or
[MIT license](LICENSE-MIT) at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
