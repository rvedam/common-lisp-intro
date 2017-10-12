#|
In Lisp in general and in Common Lisp in particular, everything is either an atom or a list of atoms. This is the basis of what we call s-expressions. S-expressions are either:

1) atoms (numbers, strings, symbols, etc.) or
2) list data structures of the (x . y) where x and y are both s-expression

Below are some examples of very simple s-expressions.
|#

1
(+ 1 2)
(* 1 2 (* 3 4))

#|
S-Expressions can also be a generic data format can be read in from a string or a file
which can then be parsed into a proper lisp object (i.e. an s-expression that we can
traverse using the standard CL list manipulation facilities like car, cdr, etc). We will touch
on this piece a bit later as it is the basis for macros.

As with other languages, Common Lisp has variables, places where you can store and hold values in your program. Common Lisp has two type of variables, lexical variables and dynamic or "special" variables.

Below are some examples of dynamic variables. Please note all variables, either dynamic or
lexical, are not typed. The value that is bound to them is typed. This is unlike other
static languages where you normally have to declare a type for a variable prior to
assignment. As the examples show below, all we need to at least define a special variable is
to define it and assign it. simple :-)
|#

(defparameter *program* "(defun hello-world () (print \"hello world\"))")
(defvar *another-program*)

;; to find the type of something we can simply call type-of
(type-of *program*)
(type-of *another-program*)


#|
Function and Functional Programming

Functions is one of key things into understanding Lisp, and Common Lisp is no exception.
Functions could be considered at the core of Lisp. In Common Lisp, there are multiple ways
of dealing with functions, and learning them will help you grasp not only how Common Lisp
deals with functions and how it implements the Functional Programming paradigm, your code
in turn will be far more modular and more composable.

Lisp was one of the first languages to support anonymous (aka lambda) functions, and in
Common Lisp that feature was inherited and implemented. 

Let's take a look at some examples of dealing with functions.
|#

;; simple way of defining a function: use symbol-function to look up the symbol and assign
;; to if it doesn't exist
(setf (symbol-function 'add) (lambda (x y) (+ x y)))

;; better way: using defun
;; below example of a sophisticated function that shows off all features of functions
(defun dummy-function (x y &key z &optional k &rest args)
  (if (and k (> (length args) 0))
      (concatenate 'list (list x y z k) args)))




;; to check if a function is bound we can use the function fboundp
(fboundp 'add2)



#|
Now dynamic variables are, in most cases, the equivalent of global variables so outside a few
cases where they are really needed most code will try and not use them. Instead, most code will
use lexical variables. These are created when you defining a function or utilize one of the
many let forms (let, let*, flet, labels).
|#

;; this is just a simple example of let
(let ((x 0))
  (+ x 1))

;; example of using flet to creat a function that simply doubles the argument passed in
(flet ((my-double (x) (* 2 x)))
  (my-double 2))

#|
Before we go on, let me just touch a little bit on comments. What you have seen so far in this
document are block comments (since I am writing significant blocks of text describing the
snippets of code below). This notation (#||#) normally only used for very extensive comments.
Other additional comment types are available. A typical practice in Common Lisp, and
what is considered canonical style is most Common Lisp code is a form of documentation where:

Four semi-colons represents a heading (like a defining a section of utilities in a file)

;;;; Utilities for convenient access in hash-tables

Three semi-colons represent a function description

;;; Mathematical sum of a function applied to a list of numbers

Two semi-colons describing the line of code below
;; common approach to collecting a number of values using loop
(loop for i from a to b collecting i)

This documentation tips become more important as you create larger codebases hence the early
warning about comment conventions.

Let's move on. 
|#
