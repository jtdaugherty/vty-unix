`vty-unix`
==========

This package provides Unix terminal support for the
[vty](https://github.com/jtdaugherty/vty) package. To use this package:

1. Add a package dependency on `vty-unix`.
2. Import `mkVty` from `Graphics.Vty.Platform.Unix`.
3. Use `mkVty` to initialize the terminal and construct a `Vty` value.
4. Use the `vty` package's API with the `Vty` value as usual.

Note: while using this package directly would make
sense for applications that support *only* Unix-based
platforms for some reason, if your intention is to
support both Windows and Unix-based platforms, depend on
[vty-crossplatform](https://github.com/jtdaugherty/vty-crossplatform)
instead in step (1) above and import `mkVty` from
`Graphics.Vty.CrossPlatform` in step (2).
