#|

 Derived Units

 Copyright (c) 2014 Odonata Research LLC

 Permission is hereby granted, free  of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction,  including without limitation the rights
 to use, copy, modify,  merge,  publish,  distribute,  sublicense, and/or sell
 copies of the  Software,  and  to  permit  persons  to  whom  the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and  this  permission  notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED  "AS IS",  WITHOUT  WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT  NOT  LIMITED  TO  THE  WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE  AND  NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT  HOLDERS  BE  LIABLE  FOR  ANY  CLAIM,  DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.

|#

(in-package :units)

;;; Dimensional analysis

(defun combine-units-p (symbol)
  "Multiple to combine units."
  (eq '* symbol))

(defun divide-units-p (symbol)
  "Divide to reduce units."
  (eq '/ symbol))

(defun unit-token-p (symbol)
  "Determine if the symbol is a unit token."
  (and
   (boundp symbol)
   (base-unit-p (symbol-value symbol))))

(defun parse-dimensions-list (dimensions-list)
  "Parse the list of dimensions to calculate the dimensions vector."
  (let ((item (first dimensions-list)))
    (cond
     ((combine-units-p item)
      (add-dimensions
       (parse-dimensions (second dimensions-list))
       (parse-dimensions (nthcdr 2 dimensions-list))))
     ((divide-units-p item)
      (subtract-dimensions
       (parse-dimensions (second dimensions-list))
       (parse-dimensions (nthcdr 2 dimensions-list))))
     ((listp item)
      (add-dimensions
       (parse-dimensions item)
       (parse-dimensions (rest dimensions-list))))
     ((unit-token-p item)
      (add-dimensions
       (dimensions item)
       (parse-dimensions (rest dimensions-list))))
     (t (error "Unknown dimension: ~A." item)))))

(defun parse-dimensions (dimensions)
  "Parse the dimensions to calculate the dimensions vector."
  (cond
   ((null dimensions) (dimension-vector :nondimensional))
   ((listp dimensions)
    (parse-dimensions-list dimensions))
   ((unit-token-p dimensions)
    (dimensions dimensions))
   (t (error "Unknown dimension: ~A." dimensions))))

;;; Derived units

(defclass derived-unit (base-unit)
  ((base-dimensions
    :type list
    :initarg :base-dimensions
    :reader base-dimensions))
  (:documentation
   "A unit derived from base units."))
