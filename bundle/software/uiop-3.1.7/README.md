UIOP, the Utilities for Implementation- and OS- Portability
===========================================================

UIOP is the portability layer of ASDF.
It provides utilities that abstract over discrepancies between implementations,
between operating systems, and between what the standard provides and
what programmers actually need, to write portable Common Lisp programs.

It is organized by topic in many files, each of which defines its own package
according to its topic: e.g [pathname.lisp](pathname.lisp)
will define package `UIOP/PATHNAME` and contain utilities related to
the handling of pathname objects.
All exported symbols are reexported in a convenience package `UIOP`,
except for those from `UIOP/COMMON-LISP`.
We recommend package `UIOP` be used to access all the symbols.

The files that constitute UIOP are, in dependency loading order:

* [package](package.lisp): deals with packages and their symbols, most notably including
  `define-package`, a variant of `defpackage` capable of hot-upgrade, or
  `symbol-call` and `find-symbol*` that are also useful for use in `.asd` files
  before packages have been defined.

* [common-lisp](common-lisp.lisp): lets you paper over various sub-standard implementations.
  Big offenders are Corman, GCL, Genera, MCL, all of them unmaintained.
  Supported without serious issues are:
  ABCL, Allegro, CCL, CMUCL, CLASP, CLISP, ECL, LispWorks, MKCL, SBCL, SCL, XCL.

* [utility](utility.lisp): provides macros and functions that do not involve I/O; it handles
  control-flow, (p)lists, characters, strings, functions, classes, conditions,
  "stamps" (real number or boolean for +/- infinity), versions, etc.
  It also sports `uiop-debug`, a useful tool to help you debug programs.

* [os](os.lisp): extracts information from your environment, including
  an ABI identifier, features that distinguish Unix vs Windows,
  `getenv`, `hostname`, `getcwd` and `chdir`, etc.

* [pathname](pathname.lisp): overcomes the gruesome non-portability trap that are CL pathnames
  (and their lovecraftian "logical" variant), offering a vast array of
  functions and a sensible, usable abstraction to specify relative pathnames.
  It has a function `merge-pathnames*` to use instead of `merge-pathnames`, or
  even better, `subpathname` and its variant `subpathname*`; it has also plenty
  of functions for dealing with pathnames being directory vs file,
  physical vs logical, absolute vs relative, and more.

* [filesystem](filesystem.lisp): provides portable access to the filesystem, inspecting it,
  only using truename when desired, using native OS namestrings,
  atomic file renaming, creating or deleting directories, etc.

* [stream](stream.lisp): portably deals with `*stderr*` vs `*error-output*`, character encodings
  (external formats), element types, safe `read`ing and `write`ing, opening files
  or temporary files, providing `format`-like designators for streams,
  flushing output buffers, consuming or copying streams, concatenating streams
  or files, copying files, etc.

* [image](image.lisp): portably deals with images, dumping them, restoring from them,
  registering hooks to run at suitable events in the image lifetime,
  printing backtraces, handling fatal conditions, using or avoiding debug modes,
  accessing command line arguments or quitting the process.

* [run-program](program.lisp): portably spawns external processes and captures their output.
  Can also capture error-output, inject input, or let it all be interactive.

* [lisp-build](lisp-build.lisp): portably compiles Common Lisp code, handles compilation results,
  muffles uninteresting conditions, saves and restores deferred warnings,
  runs hooks around compilation (to e.g. control optimizations or syntax),
  identifies the pathname of the current file, combines FASLs, etc.

* [configuration](configuration.lisp): portably locate and parse configuration files,
  using best practices to define and validate syntax, search standard paths,
  let users specify pathnames or pathname patterns, etc.

* [backward-driver](backward-driver.lisp): provides backward-compatibility
  with earlier incarnations of this library
  (i.e. ASDF internals that have leaked, ASDF-UTILS, or older versions of UIOP).

* [driver](driver.lisp): reexports all the above utilities in a single package `UIOP`.


Documentation
-------------

Each file starts with a package definition form that lists the exported symbols.

All the exported functions, macros and variables ought to have proper docstrings.
If not, then it's a legitimate bug that we invite you to report.

Maybe some automated tool will extract all that information and
make a webpage from it, at which point it would be nice to insert a link here.

One tool with which you can extract all the documentation is HEΛP.
At this time, the interface is not great: it isn't obvious at all that you can indeed
use a scrollbar on the right of the top left side panel to navigate the many packages;
once you click on the package you're interested in, you can see its defined symbols:

* <http://bimib.disco.unimib.it/people/Marco.Antoniotti/Projects/CL/HELAMBDAP/tests/asdf-uiop/docs/html/dictionary/dictionary.html>

Another automated documentation tool is quickdocs, but unhappily, at the time of this writing,
it only extracts information from the first package
(see [bug #24](https://github.com/fukamachi/quickdocs/issues/24)):

* <http://quickdocs.org/uiop/api>


Help wanted extracting working documentation from UIOP's docstrings.


Using UIOP
----------

UIOP is part of ASDF 3, and any modern Common Lisp implementation
will have all of UIOP available when you `(require "asdf")`.
NB: `(require :asdf)` also works on all implementations but CLISP.

If you need some functionality only available in a recent version of UIOP,
but cannot or will not upgrade ASDF, UIOP is also distributed separately;
see e.g. in Quicklisp. You may then have to load it like any other library:

	(asdf:load-system :uiop)

If you want to use UIOP while preserving compatibility with ASDF 2,
we recommend that in your ASDF system definition you may use the like of:

	:depends-on (#-asdf3 :uiop)


Some history
------------

UIOP, formerly known as ASDF-DRIVER (the package and system nicknames live on),
evolved from ASDF 2's internal utilities and portability layer.
It has since fully superseded functionality from the following libraries:
ASDF-UTILS (UIOP carries on the ASDF 2 utilities that this exported),
CL-FAD (UIOP's pathname and filesystem functions are much more portable),
CL-LAUNCH (UIOP took its image and command-line argument handling),
EXTERNAL-PROGRAM, TRIVIAL-SHELL and XCVB-DRIVER (UIOP's run-program evolved
from XCVB-DRIVER's, and so did its condition muffling),
SLIME's swank-loader (UIOP has better compilation and ABI identification),
TRIVIAL-BACKTRACE (UIOP/IMAGE has all of it and more), etc.

UIOP also captures a large subset of the functionality from TRIVIAL-FEATURES,
and a small subset of the functionality from ALEXANDRIA or FARE-UTILS.

We recommend you use UIOP instead of any of the above, where applicable,
since UIOP is more portable, more robust, more ubiquitous, better designed,
better documented, etc. If you see any way in which UIOP isn't superior,
we're interested in improving it so it become so.
