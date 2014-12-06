#|

 Fundamental Units

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

;;; Dimensions vector

(defun dimension-vector (dimension)
  "Return the dimension vector for the base unit."
  (flet ((dvec (len time temp mass curr subs lum)
           (make-array
            7 :element-type 'fixnum
            :initial-contents
            (list len time temp mass curr subs lum))))
    (ecase dimension
      (:length
       (dvec 1 0 0 0 0 0 0))
      (:time
       (dvec 0 1 0 0 0 0 0))
      (:temperature
       (dvec 0 0 1 0 0 0 0))
      (:mass
       (dvec 0 0 0 1 0 0 0))
      (:current
       (dvec 0 0 0 0 1 0 0))
      (:substance
       (dvec 0 0 0 0 0 1 0))
      (:luminosity
       (dvec 0 0 0 0 0 0 1))
      (:nondimensional
       (dvec 0 0 0 0 0 0 0)))))

(defun add-dimensions (dimensions1 dimensions2)
  "Return the addition of the dimension vectors."
  (loop
   with result = (dimension-vector :nondimensional)
   for index below 7 do
   (setf
    (aref result index)
    (+ (aref dimensions1 index)
       (aref dimensions2 index)))
   finally
   (return result)))

(defun subtract-dimensions (dimensions1 dimensions2)
  "Return the addition of the dimension vectors."
  (loop
   with result = (dimension-vector :nondimensional)
   for index below 7 do
   (setf
    (aref result index)
    (- (aref dimensions1 index)
       (aref dimensions2 index)))
   finally
   (return result)))

;;; Base units

;;; FIXME: Add a unit test.
(defgeneric dimensions (object)
  (:method ((object symbol))
   (dimensions (symbol-value object)))
  (:documentation
   "Return the dimension vector of the object."))

;;; FIXME: Add a unit test.
(defgeneric conversion-factor (object)
  (:method ((object symbol))
   (conversion-factor (symbol-value object)))
  (:documentation
   "Return the conversion factor of the object."))

(defclass unit ()
  ((name
    :type string
    :initarg :name
    :reader name)
   (definition
    :type string
    :initarg :definition
    :reader definition)
   (dimensions
    :type (vector 7)
    :initarg :dimensions
    :reader dimensions)
   (conversion-factor
    :type float
    :initarg :conversion-factor
    :reader conversion-factor)
   (reference-units
    :type list
    :initarg :reference-units
    :reader reference-units))
  (:documentation
   "The base class for all units with a slot for the dimension of the units.
Seven types of dimensions are handled with each corresponding to an
index of the dimensions vector.
  0:LENGTH
  1:TIME
  2:TEMPERATURE
  3:MASS
  4:CURRENT
  5:SUBSTANCE
  6:LUMINOSITY"))

(defun unitp (object)
  "Return true if the object is a base unit."
  (typep object 'unit))

(defun make-base-unit
       (name dimension conversion-factor &optional definition)
  "Return an instance of a base unit."
  (make-instance
   'unit
   :name name
   :definition (or definition "N/A")
   :dimensions (dimension-vector dimension)
   :conversion-factor conversion-factor))

(defun dimensions-equal (unit1 unit2)
  "Return true if unit1 and unit2 have equal dimensions."
  (every #'= (dimensions unit1) (dimensions unit2)))

(defparameter base-length
  (make-base-unit "LENGTH" :length 1D0)
  "Dimension vector for the length unit.")

(defparameter base-time
  (make-base-unit "TIME" :time 1D0)
  "Dimension vector for the time unit.")

(defparameter base-temperature
  (make-base-unit "TEMPERATURE" :temperature 1D0)
  "Dimension vector for the temperature unit.")

(defparameter base-mass
  (make-base-unit "MASS" :mass 1D0)
  "Dimension vector for the mass unit.")

(defparameter base-current
  (make-base-unit "CURRENT" :current 1D0)
  "Dimension vector for the current unit.")

(defparameter base-substance
  (make-base-unit "SUBSTANCE" :substance 1D0)
  "Dimension vector for the substance unit.")

(defparameter base-luminosity
  (make-base-unit "LUMINOSITY" :luminosity 1D0)
  "Dimension vector for the luminosity unit.")

(defparameter nondimensional
  (make-base-unit "NONDIMENSIONAL" :nondimensional 1D0)
  "A non-dimensional unit such as an angle.")

(define-condition unit-conversion-error (simple-error)
  ((ounit
    :type unit
    :initarg :original-unit
    :reader original-unit)
   (tunit
    :type unit
    :initarg :target-unit
    :reader target-unit)))

(defmethod print-object ((object unit-conversion-error) stream)
  "Print the unit conversion error."
  (format
   stream
   "The target unit (~A) is not compatible with the original (~A)."
   (name (target-unit object)) (name (original-unit object))))

(defun convert (value original-unit target-unit)
  "Convert the value from the original to the target unit."
  (if (dimensions-equal original-unit target-unit)
      (* value
         (/ (conversion-factor original-unit)
            (conversion-factor target-unit)))
      (error
       'unit-conversion-error
       :original-unit original-unit
       :target-unit target-unit)))

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
   (unitp (symbol-value symbol))))

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

;;; Defining base units

(defmacro define-unit
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
                    'unit
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
