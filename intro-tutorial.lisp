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

As with other languages, Common Lisp has variables, places where you can store and hold values in your program. Common Lisp has two type of variables,
lexical variables and dynamic or "special" variables.

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
Common Lisp that feature was inherited and implemented. They also support normal named
functions, as is common now in almost all languages.

Functions (both anonymous and named functions) have a several interesting features.
- They support keyword arguments
- They support optional parameters.
- They can collect all arguments passed into function as a single list to be dealt with
later (using the &rest keyword)

Let's take a look at some examples of dealing with functions.
|#

;; simple way of defining a function: use symbol-function to look up the symbol and assign
;; to if it doesn't exist
(setf (symbol-function 'add) (lambda (x y) (+ x y)))

;; better way: using defun
(defun make-triple (&key s p o)
  (list s p o))

;; If you'd like to define a function that takes an arbitrary number of arguments use the
;; &rest keyword, which will take all the arguments and pass them in as a single list structure
(defun my-sum (afun &rest args)
  (loop for i in (mapcar afun args) summing i))

;; CL also supports optional arguments
(defun example-with-optional-parameter (&optional k)
  (if k
      (print k)
      (print "k was not defined")))


;; supports default arguments as well
(defun example-with-default-argument (&optional (x 5))
  (print x))

;; this is useful to design a few utilities (this example will be used later).
(defun range (a b)
  (loop for i from a to b collecting i))

;; Of course to check if a symbol is bound to a function we can use the function fboundp
(fboundp 'add2)

#|
Let's return to dynamic variables for a moment. dynamic variables are, in most cases,
the equivalent of global variables so outside a few cases where they are really needed
most code will try and not use them. Instead, most code will
use lexical variables. These are created when you defining a function or utilize one of the
many let forms (let, let*, flet, labels). defun as well can be thought of and is a let form,
in that it creates a lexical environment and stores the variables.
|#

;; this is just a simple example of lexical environment using let form
(let ((x 0))
  (+ x 1))

;; TODO: Add let* example here during presentation

;; example of using flet to creat a function that simply doubles the argument passed in
(flet ((my-double (x) (* 2 x)))
  (my-double 2))

;; TODO: Labels Example

;; TODO: Dynamic variables 

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

Let us continue.
|#

;;; blocks, progn, tagbody
#|
Typically in Common Lisp code, code is written and sequentially, with the last expression
evaluated being the return value value of a block of code. However, there are times where
writing an explicit expression indicating a code block maybe required. For example,
in an if expression you may want multiple things to happen when a condition is satisfied.
In cases like this, you could a PROGN form (which allows you to specify several expressions
to run in sequence)

|#

(if (> 5 4)
    (progn
      (format t "5 is greater than 4~%")
      (format t "5*5 = ~A~%" (* 5 5))))

#|
In cases where we may want to return abruptly (i.e. in the middle of a code block), you can
use a block form instead. In fact, defun is often implemented using a block.
|#

(block head
  (+ 1 3)
  (return-from head 'T)
  (format t "this won't be executed"))

;; example of using tagbody

(tagbody
   (setf x 0)
 top
   (if (> x 5)
       nil
       (progn
         (format t "~A~%" x)
         (setf x (incf x))
         (go top))))


;;; iteration

;; CL supports several forms of iteration (most of which can be built using the most general
;; iteration construct in CL). to run something a certain number of times we have dotimes

(dotimes (x 5)
  (print x))

;; of course we can also dolist

(dolist (x (range 0 5))
  (print x))

;; in fact both dolist and dotimes are implemented with the do form shown below
(do ((x 0 (1+ x)))
    ((> x 5) 'T)
  (print x))

;; iterative way of implementing finding nth fibonacci number
(defun fibo-do (n)
  (do ((i 0 (1+ i))
       (x 0 y)
       (y 1 (+ x y)))
      ((> i n) y))
  )

;;; Collections and higher-order functions

;; TODO: Show mapcar

;; TODO: Show mapcan

;; TODO: Show reduce


;; Example: implementing compose 
(defun compose (&rest fns)
  (if fns
      #'(lambda (&rest args)
          (let ((fn (car (last fns)))
                (rest-fns (butlast fns)))
            (reduce #'funcall rest-fns
                    :from-end t
                    :initial-value (apply fn args))))
      #'identity))
