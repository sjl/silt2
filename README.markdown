https://itch.io/jam/august-2016-lisp-game-jam

1. Install SBCL and Quicklisp.
2. Clone or symlink <https://github.com/sjl/cl-losh> into your Quicklisp local projects directory.
3. Run `make`.
4. Run `./silt`.

Recent versions of SBCL may have broken cl-charms' Unicode support, see [this
bug](https://github.com/HiTECNOLOGYs/cl-charms/issues/32#issuecomment-1153417197).
Use an earlier version (1.8 ish) or scrub the codebase of fancy characters to
get it working until someone debugs cl-charms.
