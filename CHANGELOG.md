
0.5.0.0
=======

Enhancements:

* Overall performance has been improved due to a change from the lazy to
  CPS-based Writer monad. (thanks Andrzej Rybczak)
* Terminal titles are now UTF-8 encoded. (thanks Andrzej Rybczak)

0.4.0.0
=======

* Added support for terminfo operators "*", "/", and "m" (thanks Chris
  Forno)

0.3.0.0
=======

* Added support in `Input.Mouse` for parsing horizontal scrolling inputs
  to work with Vty 6.6.
* Improved the `vty-unix-build-width-table` tool (thanks Eric Mertens)
  to make it more robust when the terminal fails to recognize the
  `getCursorPosition` escape sequence.

0.2.0.0
=======

API changes:
* The `buildOutput` function in `Graphics.Vty.Platform.Unix.Output` now
  takes a new first argument of type `VtyUserConfig`.
* The `settingColorMode` field of `UnixSettings` was removed in favor of
  Vty 6.1's new `configPreferredColorMode` field of the `VtyUserConfig`
  type. This package now uses that setting if present; otherwise it does
  the same color mode detection that it did before this release.

0.1.0.0
=======

Initial release.
