Last Updated : 2012/06/03 23:50:18 tkych

*!!!Warning!!!*

*Current version of PreScript is 0.0.~~ (experimental alpha).*

*Current purpose of PreScript repository is to back up files.*

*Current status of this document is a mere devlopment-memo.*


PreScript: Lispized PostScript
==============================


Introduction
------------

PreScript is a DSL for generating an image in Common Lisp (or PostScript interface).

Roughly speaking, PreScript is [PostScript][] represented as S-expressions (PostScript is a registered trademark of [Adobe Systems Inc.][]).
PostScript is stack-based, postfix notation style, Turing-complete programming language with numerous operators for image generation.
PreScript generates PostScript code, and outputs the image to the appropriate viewer.
In PreScript, we can easily write code without having to worry about stack mechanism that is the cause of the complexity of the PostScript code.

For further details, please see index (Under Translation) or index-ja (Japanese) in doc directory.


  [PostScript]: http://www.adobe.com/products/postscript/
  [Adobe Systems Inc.]: http://www.adobe.com/


The Goal of PreScript
---------------------

Plain Common Lisp does not have the ability to produce an image.
Thus, when a Lisp programmer will draw a picture, he (or she) has to use another language other than Lisp.
PostScript is often used as such a language.
Because it has a very large collection of various graphics operators that enable us to draw beautiful images.
For example, in pp.339--49 of [CLtL2][], there is CL program that generates PostScript code.
It plots the behavior of mathematical functions.

The extensibility is supported by a stack mechanism, has become a source of strength for the PostScript.
In PostScript, we can define a control operator as well as a procedure, like Lisp.
However, the presence of stack mechanism is a double-edged sword.
Because, when PostScript programmer reads or writes a code, he (or she) always have to take into account the environment depending on the contents of some stacks.
PostScript is designed for a printer-machine rather than a human programmer.

Looking at this thing to replace Lisp, as if all functions and macros is depend on the values of some special variables that change from moment to moment in the course of program execution.
When a programmer reads or writes code, he (or she) must always pay attention to its values.

The goal of PreScript is to achieve the following 3 points.

1. To be able to generate images in Common Lisp.
2. Without having to be aware of the presence of stack mechanism, reading and writing of programs to become available.
3. To maintain extensibility that is supported by stack mechanism.


  [CLtL2]: http://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html


Imaging Model
-------------

Imaging model is similar to PostScript.

### User-Space ###

An user space object is an abstraction of a canvas with infinite size.
A position in the user space is specified by the value of the two-dimensional Cartesian coordinate with the origin at the lower left.

### Operators ###

An operator is an abstraction of the behavior of the painter when drawing a picture on the canvas.
A name of the operator is equivalent to the PostScript name of the operator.
The name of operator that will collide with name of Lisp operator is put on the 'y' to the end (for example, `if` -> `ify`)
All operators is closed as an operation on the user space.

### Output ###

Output operator is an abstraction of the behavior of the painter when seeing the picture.


Dependencies
------------

+ PostScript Interpreter, for examle:
     + [GhostScript](http://www.ghostscript.com/) by L. Peter Deutsch

+ PostScript Viewer, for examle:
     + [GSview](http://pages.cs.wisc.edu/~ghost/gsview/index.htm) by Russell Lang
     + [gv](http://www.gnu.org/software/gv/) by GNU, originally developed by Johannes Plass

+ [Trivial-Shell](http://common-lisp.net/project/trivial-shell/) by Gary Warren King


Installation & Start
--------------------

0.  CL-REPL> `(push "/path-to-prescript-directory/" asdf:*central-registry*)`
1.  CL-REPL> `(ql:quickload :prescript)` or `(asdf:operate 'asdf:load-op :prescript)`
2.  CL-REPL> `(in-package :prescript)`
3.  PS> 


Usage
-----

1. Make space,  `(MAKE-SPACE)`.
2. Operate space, `(NEWPATH space)`, `(MOVETO space 10 10)`, etc.
3. Output space, `(PS-OUTPUT space)` or `(PS-VIEW space)`.

All ps-operators take a space as first argument, and return the space.
For example, `(PS-OP space &rest args)` => `space`.


Examples
--------

In `(-> x &rest forms)`, macro `->` inserts x as the second item in second form,
and inserts it as the second item in third form, etc.
(`->` from [Clojure](http://clojure.org/)).

      PS> (ps-view
            (-> (make-space)
                (newpath)
                (moveto 252 324) (rlineto 72 0) (rlineto 0 72) (rlineto -72 0)
                (closepath) (filly)
                (newpath)
                (moveto 270 360) (rlineto 72 0) (rlineto 0 72) (rlineto -72 0)
                (closepath) (setgray .4) (filly)
                (newpath)
                (moveto 288 396) (rlineto 72 0) (rlineto 0 72) (rlineto -72 0)
                (closepath) (setgray .8) (filly)
                (showpage)))

      ; Output image to viewer

      PS> (defun fill-gray (space gray-scale)
            (-> space
                (setgray gray-scale)
                (filly)))

      FILL-GRAY

      PS> (ps-view
            (-> (make-space)
                (defproc box-at (x y gray-scale)
                  (newpath)
                  (moveto x y)
                  (rlineto 72 0)
                  (rlineto 0 72)
                  (rlineto -72 0)
                  (closepath)
                  (fill-gray gray-scale))
                (box-at 252 324 0)
                (box-at 270 360 .4)
                (box-at 288 396 .8)
                (showpage)))

      ; Output image to viewer (same as before, but more efficient as ps code)


Author, License, Copyright
--------------------------

* Takaya OCHIAI <tkych.repl@gmail.com>

* MIT License

* Copyright (C) 2012 Takaya OCHIAI
