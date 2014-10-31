#|

 Unit Tests for Base Units Functions

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

(in-package :test-units)

(define-test dimension-vector
  "Test the 8 valid types of dimension vectors."
  (:tag :base-unit)
  (assert-rational-equal
   #(1 0 0 0 0 0 0)
   (units::dimension-vector :length))
  (assert-rational-equal
   #(0 1 0 0 0 0 0)
   (units::dimension-vector :time))
  (assert-rational-equal
   #(0 0 1 0 0 0 0)
   (units::dimension-vector :temperature))
  (assert-rational-equal
   #(0 0 0 1 0 0 0)
   (units::dimension-vector :mass))
  (assert-rational-equal
   #(0 0 0 0 1 0 0)
   (units::dimension-vector :current))
  (assert-rational-equal
   #(0 0 0 0 0 1 0)
   (units::dimension-vector :substance))
  (assert-rational-equal
   #(0 0 0 0 0 0 1)
   (units::dimension-vector :luminosity))
  (assert-rational-equal
   #(0 0 0 0 0 0 0)
   (units::dimension-vector :nondimensional))
  (assert-error 'error (units::dimension-vector :hyper)))

(define-test base-unit
  "Test the base unit object."
  (:tag :base-unit)
  (let* ((name "testlength")
         (definition "An example unit object.")
         (dimensions (units::dimension-vector :length))
         (conversion-factor 1.2D0)
         (unit
          (make-instance
           'units::base-unit
           :name name
           :definition definition
           :dimensions dimensions
           :conversion-factor conversion-factor)))
    (assert-equal name (units:name unit))
    (assert-equal definition (units:definition unit))
    (assert-rational-equal dimensions (units::dimensions unit))
    (assert-float-equal
     conversion-factor (units:conversion-factor unit))))

(define-test base-unit-p
  "Test the base unit predicate."
  (:tag :base-unit)
  (assert-true
   (units::base-unit-p (make-instance 'units::base-unit)))
  (assert-false (units::base-unit-p 3)))

(define-test dimensions-equal
  "Test the dimensions equality predicate."
  (:tag :base-unit)
  (loop
   for dimension in
   '(:length :time :temperature :mass :current :substance :luminosity)
   as unit1 = (units::make-base-unit "UNIT1" dimension 1D0)
   as unit2 = (units::make-base-unit "UNIT2" dimension 1D0)
   do (assert-true (units::dimensions-equal unit1 unit2))))

(define-test make-base-unit
  "Test the base unit constructor."
  (:tag :base-unit)
  ;; Length
  (let ((unit (units::make-base-unit "LENGTH" :length 2D0)))
    (assert-true (typep unit 'units::base-unit))
    (assert-equal "LENGTH" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(1 0 0 0 0 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Time
  (let ((unit (units::make-base-unit "TIME" :time 2D0)))
    (assert-true (typep unit 'units::base-unit))
    (assert-equal "TIME" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 1 0 0 0 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Temperature
  (let ((unit (units::make-base-unit "TEMPERATURE" :temperature 2D0)))
    (assert-true (typep unit 'units::base-unit))
    (assert-equal "TEMPERATURE" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 1 0 0 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Mass
  (let ((unit (units::make-base-unit "MASS" :mass 2D0)))
    (assert-true (typep unit 'units::base-unit))
    (assert-equal "MASS" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 0 1 0 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Current
  (let ((unit (units::make-base-unit "CURRENT" :current 2D0)))
    (assert-true (typep unit 'units::base-unit))
    (assert-equal "CURRENT" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 0 0 1 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Substance
  (let ((unit (units::make-base-unit "SUBSTANCE" :substance 2D0)))
    (assert-true (typep unit 'units::base-unit))
    (assert-equal "SUBSTANCE" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 0 0 0 1 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Luminosity
  (let ((unit (units::make-base-unit "LUMINOSITY" :luminosity 2D0)))
    (assert-true (typep unit 'units::base-unit))
    (assert-equal "LUMINOSITY" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 0 0 0 0 1) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit))))

(define-test convert
  "Test the fundamental unit conversion function."
  (:tag :base-unit)
  (let ((meter (units::make-base-unit "METER" :length 1D0))
        (foot (units::make-base-unit "FOOT" :length 3.048D-1))
        (inch (units::make-base-unit "INCH" :length 2.540D-2)))
    (loop
     for mval from 1D0 to 5D0 by 2D-1
     as fval = (/ mval 3.048D-1)
     as ival = (/ mval 2.540D-2)
     do
     ;; Convert meter & inch to foot
     (assert-float-equal fval (units:convert mval meter foot))
     (assert-float-equal fval (units:convert ival inch foot))
     ;; Convert meter & foot to inch
     (assert-float-equal ival (units:convert mval meter inch))
     (assert-float-equal ival (units:convert fval foot inch)))))
