#|

 Unit Test for Derived Units

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

(define-test combine-units-p
  "Test for the asterisk (*)."
  (:tag :derived-units :parsing)
  (assert-true (units::combine-units-p '*))
  (assert-false (units::combine-units-p '/))
  (assert-false (units::combine-units-p 'meter)))

(define-test divide-units-p
  "Test for the slash (/)."
  (:tag :derived-units :parsing)
  (assert-true (units::divide-units-p '/))
  (assert-false (units::divide-units-p '*))
  (assert-false (units::divide-units-p 'meter)))

(define-test unit-token-p
  "Test for a unit token."
  (:tag :derived-units :parsing)
  (assert-true (units::unit-token-p 'units:meter))
  (assert-true (units::unit-token-p 'units:inch))
  (assert-false (units::unit-token-p 'hogshead)))

(define-test parse-dimensions-list
  "Test the dimensions parsing function for lists."
  (:tag :derived-units :parsing)
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
  (:tag :derived-units :parsing)
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
