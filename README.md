# xlisp
## An object-oriented LISP

Version 3.0

February 18, 2006

## Building with CMake
We've added the ability to build with CMake to simplify building xlisp on your
system. The way that we expect this to work on Linux systems using `make` would
be to first make a build directory. For this walkthrough we'll say that we
start _in_ the xlisp directory:

```bash
cd ..
mkdir build
cd build
ccmake ../xlisp
```
So, now we have made a build directory outside of xlisp, so that the build
products don't get strewn all over our pristine source. The `ccmake` command is
a curses front end to CMake that I like. In `ccmake` you would first
_configure_ (with "c"), then select your build type (typically "Release"), then
_configure_ again, then _generate_ (with "g").  You will then be back in your
shell, where makefiles have been set up for you. (CMake isn't constrained to
only makefiles, but can also be instructed to generate most other kinds of
build files).  After that you can:

```bash
make
# and then, either:
make install
# or
make package
```

With the CMake file we have in there, it also has a "package" target, which
will most likely result in a gzipped tar file of the build products. It is also
possible to alter the `CMakeLists.txt` file to generate other package types,
such as `*.rpm`, `*.deb`, etc.

#### David Michael Betz

18 Garrison Drive
Bedford, US, NH 03110

(603) 472-2389 (home)

#### Copyright (c) 1984-2006, by David Michael Betz

All Rights Reserved

See the included file `LICENSE.txt` for the full license.
