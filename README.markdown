Last Updated : 2012/05/31 00:12:33 tkych


*!!!Warning!!!*

*Current version of PreScript is 0.0.06 (experimental alpha).*

*Current purpose of PreScript repository is to back up files.*

*Current status of this document is a mere devlopment-memo.*


# PreScript: Lispized PostScript


## Introduction

PreScript is a DSL for generating an image in Common Lisp.
Roughly speaking, PreScript is PostScript represented by S-expressions
 (PostScript is a registered trademark of Adobe Systems Inc.).
PostScript is stack-oriented, Turing-complete programming language with numerous operators for image generation.
PreScript generates PostScript code, and outputs the image to the appropriate viewer.
In PreScript, we can easily write code without having to worry about stack mechanism that is the cause of the complexity of the PostScript code.

For further details, please see documents index(Under Translation) or index-ja(Japanese) in doc directory.


## The Goal of PreScript

Plain Common Lisp does not have the ability to produce an image.
Thus, when a Lisp programmer draws a picture, he (or she) has to use another language other than Lisp.
PostScript is often used as such a language.
Because it is  
For example, in pp.339--49 of CLtL2, there is CL program that generates PostScript code.
It plots the behavior of mathematical functions.

The extensibility is supported by a stack mechanism, has become a source of strength for the PostScript.
However, the presence of stack mechanism is a double-edged sword.
Because, when PostScript programmer reads or writes a code,
he (or she) always have to take into account the environment depending on the contents of some stacks.
PostScript is designed for a machine rather than a human programmer.

Looking at this thing to replace Lisp,
as if all functions and macros is depend on the value of special variable that change from moment to moment in the course of program execution.
When a programmer reads or write code, he (or she) must always pay attention to its value.


The goal of PreScript is to achieve the following three points.

1. To be able to generate images in Common Lisp.
2. Without having to be aware of the presence of stack mechanism, reading and writing of programs to become available.
3. To maintain extensibility that is supported by stack mechanism.


## Dependencies

* PostScript Interpreter, for examle:
     * [GhostScript](http://www.ghostscript.com/) by L. Peter Deutsch

* PostScript Viewer, for examle:
     * [GSview](http://pages.cs.wisc.edu/~ghost/gsview/index.htm) by Russell Lang
     * [gv](http://www.gnu.org/software/gv/) by GNU, originally developed by Johannes Plass

* [Trivial-Shell](http://common-lisp.net/project/trivial-shell/) by Gary Warren King


## Installation & Start

0.  CL-REPL> `(push "/path-to-prescript-directory/" asdf:*central-registry*)`
1.  CL-REPL> `(ql:quickload :prescript)` or `(asdf:operate 'asdf:load-op :prescript)`
2.  CL-REPL> `(in-package :prescript)`
3.  PS> 


## Usage


## Examples

`->` is from Clojure

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

      ; Output image (same as before) to viewer


## Author, License, Copyright

* Takaya OCHIAI <tkych.repl@gmail.com>

* MIT License

* Copyright (C) 2012 Takaya OCHIAI
