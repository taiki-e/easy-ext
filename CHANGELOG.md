# Unreleased

# 0.1.6 - 2019-10-12

* Improved error messages related to visibility.

# 0.1.5 - 2019-08-15

* Updated `syn` and `quote` to 1.0.

# 0.1.4 - 2019-03-10

* Updated minimum `syn` version to 0.15.29.

# 0.1.3 - 2019-02-21

* Removed `inline` attributes on trait method side. It can avoid [`clippy::inline_fn_without_body`](https://rust-lang.github.io/rust-clippy/master/index.html#inline_fn_without_body) by this.

# 0.1.2 - 2019-02-21

* Used `#[allow(patterns_in_fns_without_body)]` to generated extension trait.

* Fixed some bugs related to generics.

# 0.1.1 - 2019-02-21

* Fixed an error related to generics.

# 0.1.0 - 2019-02-20

Initial release
