# easy-ext

[![crates-badge]][crates-url]
[![docs-badge]][docs-url]
[![license-badge]][license]
[![rustc-badge]][rustc-url]

[crates-badge]: https://img.shields.io/crates/v/easy-ext.svg
[crates-url]: https://crates.io/crates/easy-ext
[docs-badge]: https://docs.rs/easy-ext/badge.svg
[docs-url]: https://docs.rs/easy-ext
[license-badge]: https://img.shields.io/badge/license-Apache--2.0%20OR%20MIT-blue.svg
[license]: #license
[rustc-badge]: https://img.shields.io/badge/rustc-1.31+-lightgray.svg
[rustc-url]: https://blog.rust-lang.org/2018/12/06/Rust-1.31-and-rust-2018.html

An attribute macro for easily writing [extension trait pattern](https://github.com/rust-lang/rfcs/blob/master/text/0445-extension-trait-conventions.md).

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
easy-ext = "0.2"
```

The current easy-ext requires Rust 1.31 or later.

## Examples

```rust
use easy_ext::ext;

#[ext(ResultExt)]
impl<T, E> Result<T, E> {
    pub fn err_into<U>(self) -> Result<T, U>
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

You can elide the trait name. Note that in this case, `#[ext]` assigns a random name, so you cannot import/export the generated trait.

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

See [documentation][docs-url] for more details.

## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
* MIT license ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
