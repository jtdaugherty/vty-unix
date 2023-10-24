`vty-unix`
==========

This package provides Unix terminal support for the
[vty](https://github.com/jtdaugherty/vty) package. To use this package:

1. Add package dependencies on `vty-unix` and `vty >= 6.0`.
2. Import `mkVty` from `Graphics.Vty.Platform.Unix`.
3. Use `mkVty` to initialize the terminal and construct a `Vty` value.
4. Use the `vty` package's API with the `Vty` value as usual.

Note: while using this package directly would make
sense for applications that support *only* Unix-based
platforms for some reason, if your intention is to
support both Windows and Unix-based platforms, depend on
[vty-crossplatform](https://github.com/jtdaugherty/vty-crossplatform)
instead of `vty-unix` in step (1) above and import `mkVty` from
`Graphics.Vty.CrossPlatform` in step (2).

# Features

* Supports a large number of terminals, i.e., vt100, ansi, hurd, linux,
  `screen`, etc., or anything with a sufficient terminfo entry.

* Supports Unicode output on terminals with UTF-8 support.

* Automatically decodes keyboard keys into (key,[modifier]) tuples.

* Supports a keypress timeout after for lone ESC. The timeout is
  customizable.

* Supports ANSI graphics modes (SGR as defined in `console_codes(4)`)
  with a type-safe interface and graceful fallback for terminals
  with limited or nonexistent support for such modes.

* Supports "normal" and "extended" (SGR) mouse modes as described at
  http://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-Mouse-Tracking

* Supports bracketed paste mode as described at
  http://cirw.in/blog/bracketed-paste

* Supports multi-column Unicode characters such as emoji characters. In
  cases where Vty and your terminal emulator disagree on character
  widths, Vty provides a tool `vty-build-width-table` and library
  functionality to build a width table that will work for your terminal
  and load it on application startup.
