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

(defun parse-dimensions-list (units-list)
  "Parse the list of dimensions to calculate the dimensions vector."
  (let ((item (first units-list)))
    (cond
     ((combine-units-p item)
      (add-dimensions
       (parse-dimensions (second units-list))
       (parse-dimensions (nthcdr 2 units-list))))
     ((divide-units-p item)
      (subtract-dimensions
       (parse-dimensions (second units-list))
       (parse-dimensions (nthcdr 2 units-list))))
     ((listp item)
      (add-dimensions
       (parse-dimensions item)
       (parse-dimensions (rest units-list))))
     ((unit-token-p item)
      (add-dimensions
       (dimensions item)
       (parse-dimensions (rest units-list))))
     (t (error "Unknown dimension: ~A." item)))))

(defun parse-dimensions (units)
  "Parse the dimensions to calculate the dimensions vector."
  (cond
   ((null units) (dimension-vector :nondimensional))
   ((listp units)
    (parse-dimensions-list units))
   ((unit-token-p units)
    (dimensions units))
   (t (error "Unknown dimension: ~A." units))))

;;; FIXME: Use the LOOP to walk the list of units and wrap units with
;;; (conversion-factor unit)
(defun parse-conversion-factor-list (units-list)
  "Parse the list of dimensions to calculate the conversion factor."
  (let ((item (first units-list)))
    (cond
     ((combine-units-p item)
      (* (parse-conversion-factor (second units-list))
         (parse-conversion-factor (nthcdr 2 units-list))))
     ((divide-units-p item)
      (/ (parse-conversion-factor (second units-list))
         (parse-conversion-factor (nthcdr 2 units-list))))
     ((listp item)
      (* (parse-conversion-factor item)
         (parse-conversion-factor (rest units-list))))
     ((unit-token-p item)
      (* (conversion-factor item)
         (parse-conversion-factor (rest units-list))))
     (t (error "Unknown dimension: ~A." item)))))

(defun parse-conversion-factor (units)
  "Parse the conversion factor based on the units."
  (cond
   ((null units) 1D0)
   ((listp units)
    (parse-conversion-factor-list units))
   ((unit-token-p units)
    (conversion-factor units))
   (t (error "Unknown dimension: ~A." units))))

;;; Derived units

(defclass derived-unit (base-unit)
  ((reference-units
    :type list
    :initarg :reference-units
    :reader reference-units))
  (:documentation
   "A unit derived from base units."))

;;; Defining derived units

(defmacro define-derived-unit
          (name reference-units &optional conversion-factor definition)
  "Define the unit with name."
  (if (symbolp (quote name))
      (let ((refunit (gensym "REFUNIT-"))
            (unit (gensym "UNIT-")))
        `(progn
           (declaim (special ,name))
           (let* ((,refunit ',reference-units)
                  (,unit
                   (make-instance
                    'derived-unit
                    :name (symbol-name ',name)
                    :definition (or ,definition "N/A")
                    :dimensions (parse-dimensions ,refunit)
                    :reference-units ,refunit
                    :conversion-factor
                    (* (parse-conversion-factor ,refunit)
                       (or ,conversion-factor 1D0)))))
             (setf
              ;; Unit instance
              (symbol-value ',name) ,unit
              ;; Unit conversion
              (symbol-function ',name)
              (lambda (value original-unit)
                (convert value original-unit ,unit))))
           ;; Return the name
           ',name))
      (error "Unit name ~A is not of type SYMBOL." name)))
