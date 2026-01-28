# Checks if a package is installed and *informs* the user if not

This is wrapper around
[rlang::check_installed](https://rlang.r-lib.org/reference/is_installed.html);
instead of erroring out if the check fails it returns `FALSE`. However,
unlike
[rlang::is_installed](https://rlang.r-lib.org/reference/is_installed.html),
it emits a message to the user.

## Usage

``` r
spod_assert_package(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`rlang::check_installed`](https://rlang.r-lib.org/reference/is_installed.html)

  `pkg`

  :   The package names. Can include version requirements, e.g.
      `"pkg (>= 1.0.0)"`.

  `version`

  :   Minimum versions for `pkg`. If supplied, must be the same length
      as `pkg`. `NA` elements stand for any versions.

  `compare`

  :   A character vector of comparison operators to use for `version`.
      If supplied, must be the same length as `version`. If `NULL`, `>=`
      is used as default for all elements. `NA` elements in `compare`
      are also set to `>=` by default.

  `reason`

  :   Optional string indicating why is `pkg` needed. Appears in error
      messages (if non-interactive) and user prompts (if interactive).

  `action`

  :   An optional function taking `pkg` and `...` arguments. It is
      called by `check_installed()` when the user chooses to update
      outdated packages. The function is passed the missing and outdated
      packages as a character vector of names.

  `call`

  :   The execution environment of a currently running function, e.g.
      `caller_env()`. The function will be mentioned in error messages
      as the source of the error. See the `call` argument of
      [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
      information.
