#|

 Unit Tests for Fundamental Units Functions

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
  (:tag :dimensions)
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

(define-test add-dimensions
  "Test addition of unit dimension vectors."
  (:tag :dimensions)
  (loop
   for (dimension1 dimension2 result) in
   '((:LENGTH :LENGTH #(2 0 0 0 0 0 0))
     (:LENGTH :TIME #(1 1 0 0 0 0 0))
     (:LENGTH :TEMPERATURE #(1 0 1 0 0 0 0))
     (:LENGTH :MASS #(1 0 0 1 0 0 0))
     (:LENGTH :CURRENT #(1 0 0 0 1 0 0))
     (:LENGTH :SUBSTANCE #(1 0 0 0 0 1 0))
     (:LENGTH :LUMINOSITY #(1 0 0 0 0 0 1))
     (:LENGTH :NONDIMENSIONAL #(1 0 0 0 0 0 0))
     (:TIME :LENGTH #(1 1 0 0 0 0 0))
     (:TIME :TIME #(0 2 0 0 0 0 0))
     (:TIME :TEMPERATURE #(0 1 1 0 0 0 0))
     (:TIME :MASS #(0 1 0 1 0 0 0))
     (:TIME :CURRENT #(0 1 0 0 1 0 0))
     (:TIME :SUBSTANCE #(0 1 0 0 0 1 0))
     (:TIME :LUMINOSITY #(0 1 0 0 0 0 1))
     (:TIME :NONDIMENSIONAL #(0 1 0 0 0 0 0))
     (:TEMPERATURE :LENGTH #(1 0 1 0 0 0 0))
     (:TEMPERATURE :TIME #(0 1 1 0 0 0 0))
     (:TEMPERATURE :TEMPERATURE #(0 0 2 0 0 0 0))
     (:TEMPERATURE :MASS #(0 0 1 1 0 0 0))
     (:TEMPERATURE :CURRENT #(0 0 1 0 1 0 0))
     (:TEMPERATURE :SUBSTANCE #(0 0 1 0 0 1 0))
     (:TEMPERATURE :LUMINOSITY #(0 0 1 0 0 0 1))
     (:TEMPERATURE :NONDIMENSIONAL #(0 0 1 0 0 0 0))
     (:MASS :LENGTH #(1 0 0 1 0 0 0))
     (:MASS :TIME #(0 1 0 1 0 0 0))
     (:MASS :TEMPERATURE #(0 0 1 1 0 0 0))
     (:MASS :MASS #(0 0 0 2 0 0 0))
     (:MASS :CURRENT #(0 0 0 1 1 0 0))
     (:MASS :SUBSTANCE #(0 0 0 1 0 1 0))
     (:MASS :LUMINOSITY #(0 0 0 1 0 0 1))
     (:MASS :NONDIMENSIONAL #(0 0 0 1 0 0 0))
     (:CURRENT :LENGTH #(1 0 0 0 1 0 0))
     (:CURRENT :TIME #(0 1 0 0 1 0 0))
     (:CURRENT :TEMPERATURE #(0 0 1 0 1 0 0))
     (:CURRENT :MASS #(0 0 0 1 1 0 0))
     (:CURRENT :CURRENT #(0 0 0 0 2 0 0))
     (:CURRENT :SUBSTANCE #(0 0 0 0 1 1 0))
     (:CURRENT :LUMINOSITY #(0 0 0 0 1 0 1))
     (:CURRENT :NONDIMENSIONAL #(0 0 0 0 1 0 0))
     (:SUBSTANCE :LENGTH #(1 0 0 0 0 1 0))
     (:SUBSTANCE :TIME #(0 1 0 0 0 1 0))
     (:SUBSTANCE :TEMPERATURE #(0 0 1 0 0 1 0))
     (:SUBSTANCE :MASS #(0 0 0 1 0 1 0))
     (:SUBSTANCE :CURRENT #(0 0 0 0 1 1 0))
     (:SUBSTANCE :SUBSTANCE #(0 0 0 0 0 2 0))
     (:SUBSTANCE :LUMINOSITY #(0 0 0 0 0 1 1))
     (:SUBSTANCE :NONDIMENSIONAL #(0 0 0 0 0 1 0))
     (:LUMINOSITY :LENGTH #(1 0 0 0 0 0 1))
     (:LUMINOSITY :TIME #(0 1 0 0 0 0 1))
     (:LUMINOSITY :TEMPERATURE #(0 0 1 0 0 0 1))
     (:LUMINOSITY :MASS #(0 0 0 1 0 0 1))
     (:LUMINOSITY :CURRENT #(0 0 0 0 1 0 1))
     (:LUMINOSITY :SUBSTANCE #(0 0 0 0 0 1 1))
     (:LUMINOSITY :LUMINOSITY #(0 0 0 0 0 0 2))
     (:LUMINOSITY :NONDIMENSIONAL #(0 0 0 0 0 0 1))
     (:NONDIMENSIONAL :LENGTH #(1 0 0 0 0 0 0))
     (:NONDIMENSIONAL :TIME #(0 1 0 0 0 0 0))
     (:NONDIMENSIONAL :TEMPERATURE #(0 0 1 0 0 0 0))
     (:NONDIMENSIONAL :MASS #(0 0 0 1 0 0 0))
     (:NONDIMENSIONAL :CURRENT #(0 0 0 0 1 0 0))
     (:NONDIMENSIONAL :SUBSTANCE #(0 0 0 0 0 1 0))
     (:NONDIMENSIONAL :LUMINOSITY #(0 0 0 0 0 0 1))
     (:NONDIMENSIONAL :NONDIMENSIONAL #(0 0 0 0 0 0 0)))
   do
   (assert-rational-equal
    result
    (units::add-dimensions
     (units::dimension-vector dimension1)
     (units::dimension-vector dimension2))
    dimension1
    dimension2)))

(define-test subtract-dimensions
  "Test subtraction of the unit dimension vector."
  (:tag :dimensions)
  (loop
   for (dimension1 dimension2 result) in
   '((:LENGTH :LENGTH #(0 0 0 0 0 0 0))
     (:LENGTH :TIME #(1 -1 0 0 0 0 0))
     (:LENGTH :TEMPERATURE #(1 0 -1 0 0 0 0))
     (:LENGTH :MASS #(1 0 0 -1 0 0 0))
     (:LENGTH :CURRENT #(1 0 0 0 -1 0 0))
     (:LENGTH :SUBSTANCE #(1 0 0 0 0 -1 0))
     (:LENGTH :LUMINOSITY #(1 0 0 0 0 0 -1))
     (:LENGTH :NONDIMENSIONAL #(1 0 0 0 0 0 0))
     (:TIME :LENGTH #(-1 1 0 0 0 0 0))
     (:TIME :TIME #(0 0 0 0 0 0 0))
     (:TIME :TEMPERATURE #(0 1 -1 0 0 0 0))
     (:TIME :MASS #(0 1 0 -1 0 0 0))
     (:TIME :CURRENT #(0 1 0 0 -1 0 0))
     (:TIME :SUBSTANCE #(0 1 0 0 0 -1 0))
     (:TIME :LUMINOSITY #(0 1 0 0 0 0 -1))
     (:TIME :NONDIMENSIONAL #(0 1 0 0 0 0 0))
     (:TEMPERATURE :LENGTH #(-1 0 1 0 0 0 0))
     (:TEMPERATURE :TIME #(0 -1 1 0 0 0 0))
     (:TEMPERATURE :TEMPERATURE #(0 0 0 0 0 0 0))
     (:TEMPERATURE :MASS #(0 0 1 -1 0 0 0))
     (:TEMPERATURE :CURRENT #(0 0 1 0 -1 0 0))
     (:TEMPERATURE :SUBSTANCE #(0 0 1 0 0 -1 0))
     (:TEMPERATURE :LUMINOSITY #(0 0 1 0 0 0 -1))
     (:TEMPERATURE :NONDIMENSIONAL #(0 0 1 0 0 0 0))
     (:MASS :LENGTH #(-1 0 0 1 0 0 0))
     (:MASS :TIME #(0 -1 0 1 0 0 0))
     (:MASS :TEMPERATURE #(0 0 -1 1 0 0 0))
     (:MASS :MASS #(0 0 0 0 0 0 0))
     (:MASS :CURRENT #(0 0 0 1 -1 0 0))
     (:MASS :SUBSTANCE #(0 0 0 1 0 -1 0))
     (:MASS :LUMINOSITY #(0 0 0 1 0 0 -1))
     (:MASS :NONDIMENSIONAL #(0 0 0 1 0 0 0))
     (:CURRENT :LENGTH #(-1 0 0 0 1 0 0))
     (:CURRENT :TIME #(0 -1 0 0 1 0 0))
     (:CURRENT :TEMPERATURE #(0 0 -1 0 1 0 0))
     (:CURRENT :MASS #(0 0 0 -1 1 0 0))
     (:CURRENT :CURRENT #(0 0 0 0 0 0 0))
     (:CURRENT :SUBSTANCE #(0 0 0 0 1 -1 0))
     (:CURRENT :LUMINOSITY #(0 0 0 0 1 0 -1))
     (:CURRENT :NONDIMENSIONAL #(0 0 0 0 1 0 0))
     (:SUBSTANCE :LENGTH #(-1 0 0 0 0 1 0))
     (:SUBSTANCE :TIME #(0 -1 0 0 0 1 0))
     (:SUBSTANCE :TEMPERATURE #(0 0 -1 0 0 1 0))
     (:SUBSTANCE :MASS #(0 0 0 -1 0 1 0))
     (:SUBSTANCE :CURRENT #(0 0 0 0 -1 1 0))
     (:SUBSTANCE :SUBSTANCE #(0 0 0 0 0 0 0))
     (:SUBSTANCE :LUMINOSITY #(0 0 0 0 0 1 -1))
     (:SUBSTANCE :NONDIMENSIONAL #(0 0 0 0 0 1 0))
     (:LUMINOSITY :LENGTH #(-1 0 0 0 0 0 1))
     (:LUMINOSITY :TIME #(0 -1 0 0 0 0 1))
     (:LUMINOSITY :TEMPERATURE #(0 0 -1 0 0 0 1))
     (:LUMINOSITY :MASS #(0 0 0 -1 0 0 1))
     (:LUMINOSITY :CURRENT #(0 0 0 0 -1 0 1))
     (:LUMINOSITY :SUBSTANCE #(0 0 0 0 0 -1 1))
     (:LUMINOSITY :LUMINOSITY #(0 0 0 0 0 0 0))
     (:LUMINOSITY :NONDIMENSIONAL #(0 0 0 0 0 0 1))
     (:NONDIMENSIONAL :LENGTH #(-1 0 0 0 0 0 0))
     (:NONDIMENSIONAL :TIME #(0 -1 0 0 0 0 0))
     (:NONDIMENSIONAL :TEMPERATURE #(0 0 -1 0 0 0 0))
     (:NONDIMENSIONAL :MASS #(0 0 0 -1 0 0 0))
     (:NONDIMENSIONAL :CURRENT #(0 0 0 0 -1 0 0))
     (:NONDIMENSIONAL :SUBSTANCE #(0 0 0 0 0 -1 0))
     (:NONDIMENSIONAL :LUMINOSITY #(0 0 0 0 0 0 -1))
     (:NONDIMENSIONAL :NONDIMENSIONAL #(0 0 0 0 0 0 0)))
   do
   (assert-rational-equal
    result
    (units::subtract-dimensions
     (units::dimension-vector dimension1)
     (units::dimension-vector dimension2))
    dimension1
    dimension2)))

(define-test unit
  "Test the fundamental unit object."
  (:tag :fundamental)
  (let* ((name "testlength")
         (definition "An example unit object.")
         (dimensions (units::dimension-vector :length))
         (conversion-factor 1.2D0)
         (unit
          (make-instance
           'units::unit
           :name name
           :definition definition
           :dimensions dimensions
           :conversion-factor conversion-factor)))
    (assert-equal name (units:name unit))
    (assert-equal definition (units:definition unit))
    (assert-rational-equal dimensions (units::dimensions unit))
    (assert-float-equal
     conversion-factor (units:conversion-factor unit))))

(define-test unitp
  "Test the base unit predicate."
  (:tag :fundamental)
  (assert-true (units::unitp (make-instance 'units::unit)))
  (assert-false (units::unitp 3)))

(define-test dimensions-equal
  "Test the dimensions equality predicate."
  (:tag :fundamental)
  (loop
   for dimension in
   '(:length :time :temperature :mass :current :substance :luminosity)
   as unit1 = (units::make-base-unit "UNIT1" dimension 1D0)
   as unit2 = (units::make-base-unit "UNIT2" dimension 1D0)
   do (assert-true (units::dimensions-equal unit1 unit2))))

(define-test make-base-unit
  "Test the base unit constructor."
  (:tag :fundamental)
  ;; Length
  (let ((unit (units::make-base-unit "LENGTH" :length 2D0)))
    (assert-true (typep unit 'units::unit))
    (assert-equal "LENGTH" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(1 0 0 0 0 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Time
  (let ((unit (units::make-base-unit "TIME" :time 2D0)))
    (assert-true (typep unit 'units::unit))
    (assert-equal "TIME" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 1 0 0 0 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Temperature
  (let ((unit (units::make-base-unit "TEMPERATURE" :temperature 2D0)))
    (assert-true (typep unit 'units::unit))
    (assert-equal "TEMPERATURE" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 1 0 0 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Mass
  (let ((unit (units::make-base-unit "MASS" :mass 2D0)))
    (assert-true (typep unit 'units::unit))
    (assert-equal "MASS" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 0 1 0 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Current
  (let ((unit (units::make-base-unit "CURRENT" :current 2D0)))
    (assert-true (typep unit 'units::unit))
    (assert-equal "CURRENT" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 0 0 1 0 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Substance
  (let ((unit (units::make-base-unit "SUBSTANCE" :substance 2D0)))
    (assert-true (typep unit 'units::unit))
    (assert-equal "SUBSTANCE" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 0 0 0 1 0) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit)))
  ;; Luminosity
  (let ((unit (units::make-base-unit "LUMINOSITY" :luminosity 2D0)))
    (assert-true (typep unit 'units::unit))
    (assert-equal "LUMINOSITY" (units:name unit))
    (assert-equal "N/A" (units:definition unit))
    (assert-rational-equal
     #(0 0 0 0 0 0 1) (units::dimensions unit))
    (assert-float-equal 2D0 (units:conversion-factor unit))))

(define-test convert
  "Test the fundamental unit conversion function."
  (:tag :fundamental)
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

(define-test combine-units-p
  "Test for the asterisk (*)."
  (:tag :fundamental :parsing)
  (assert-true (units::combine-units-p '*))
  (assert-false (units::combine-units-p '/))
  (assert-false (units::combine-units-p 'meter)))

(define-test divide-units-p
  "Test for the slash (/)."
  (:tag :fundamental :parsing)
  (assert-true (units::divide-units-p '/))
  (assert-false (units::divide-units-p '*))
  (assert-false (units::divide-units-p 'meter)))

(define-test unit-token-p
  "Test for a unit token."
  (:tag :fundamental :parsing)
  (assert-true (units::unit-token-p 'units:meter))
  (assert-true (units::unit-token-p 'units:inch))
  (assert-false (units::unit-token-p 'hogshead)))

(define-test parse-dimensions-list
  "Test the dimensions parsing function for lists."
  (:tag :fundamental :parsing)
  ;; Combination
  (assert-rational-equal
   #(2 0 0 0 0 0 0)
   (units::parse-dimensions-list '(* units:meter units:meter)))
  (assert-rational-equal
   #(0 0 0 2 0 0 0)
   (units::parse-dimensions-list '(* units:kilogram units:kilogram)))
  ;; Division
  (assert-rational-equal
   #(1 -2 0 0 0 0 0)
   (units::parse-dimensions-list
    '(/ units:meter units:seconds units:seconds)))
  (assert-rational-equal
   #(1 -2 0 0 0 0 0)
   (units::parse-dimensions-list
    '(/ units:meter (* units:seconds units:seconds))))
  ;; Rest
  (assert-rational-equal
   #(2 0 0 0 0 0 0)
   (units::parse-dimensions-list '(units:meter units:meter)))
  (assert-rational-equal
   #(0 0 0 2 0 0 0)
   (units::parse-dimensions-list '(units:kilogram units:kilogram)))
  ;; Unit token
  (assert-rational-equal
   #(1 0 0 0 0 0 0) (units::parse-dimensions-list '(units:meter)))
  (assert-rational-equal
   #(0 1 0 0 0 0 0) (units::parse-dimensions-list '(units:seconds))))

(define-test parse-dimensions
  "Test the top-level dimensions parsing function."
  (:tag :fundamental :parsing)
  ;; Nondimensional
  (assert-rational-equal
   #(0 0 0 0 0 0 0) (units::parse-dimensions ()))
  ;; Parse a list
  (assert-rational-equal
   #(1 -2 0 0 0 0 0)
   (units::parse-dimensions
    '(/ units:meter (* units:seconds units:seconds))))
  ;; Parse a unit
  (assert-rational-equal
   #(1 0 0 0 0 0 0) (units::parse-dimensions 'units:meter))
  (assert-rational-equal
   #(0 0 0 1 0 0 0) (units::parse-dimensions 'units:kilogram)))
