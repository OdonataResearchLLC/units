#|

 Unit tests for United States Customary System of Units

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

;;; Units of length

(define-test us-customary-length
  (:tag :us :length)
  ;; Inch
  (assert-float-equal (/ 2.54D-2) (units:inch 1D0 units:meter))
  (assert-float-equal 12D0 (units:inch 1D0 units:foot))
  (assert-float-equal 36D0 (units:inch 1D0 units:yard))
  (assert-float-equal 6.336D4 (units:inch 1D0 units:mile))
  (assert-float-equal
   72913.38582677166D0 (units:inch 1D0 units:nautical-mile))
  ;; Foot
  (assert-float-equal (/ 3.048D-1) (units:foot 1D0 units:meter))
  (assert-float-equal (/ 12D0) (units:foot 1D0 units:inch))
  (assert-float-equal 3D0 (units:foot 1D0 units:yard))
  (assert-float-equal 5.28D3 (units:foot 1D0 units:mile))
  (assert-float-equal
   6076.115485564304D0 (units:foot 1D0 units:nautical-mile))
  ;; Yard
  (assert-float-equal (/ 9.144D-1) (units:yard 1D0 units:meter))
  (assert-float-equal (/ 36D0) (units:yard 1D0 units:inch))
  (assert-float-equal (/ 3D0) (units:yard 1D0 units:foot))
  (assert-float-equal 1.76D3 (units:yard 1D0 units:mile))
  (assert-float-equal
   2025.3718285214348D0 (units:yard 1D0 units:nautical-mile))
  ;; Mile
  (assert-float-equal (/ 1.609344D3) (units:mile 1D0 units:meter))
  (assert-float-equal (/ 6.336D4) (units:mile 1D0 units:inch))
  (assert-float-equal (/ 5.28D3) (units:mile 1D0 units:foot))
  (assert-float-equal (/ 1.76D3) (units:mile 1D0 units:yard))
  (assert-float-equal
   1.1507794480235425D0 (units:mile 1D0 units:nautical-mile))
  ;; Nautical mile
  (assert-float-equal
   (/ 1.852D3) (units:nautical-mile 1D0 units:meter))
  (assert-float-equal
   (/ 72913.38582677166D0) (units:nautical-mile 1D0 units:inch))
  (assert-float-equal
   (/ 6076.115485564304D0) (units:nautical-mile 1D0 units:foot))
  (assert-float-equal
   (/ 2025.3718285214348D0) (units:nautical-mile 1D0 units:yard))
  (assert-float-equal
   (/ 1.1507794480235425D0) (units:nautical-mile 1D0 units:mile)))

;;; Units of mass

(define-test us-customary-mass
  (:tag :us :mass)
  ;; Ounce
  (assert-float-equal
   (/ 28.349523125D-3) (units:ounce 1D0 units:kilogram))
  (assert-float-equal 16D0 (units:ounce 1D0 units:pound))
  (assert-float-equal 32D3 (units:ounce 1D0 units:ton))
  ;; Pound
  (assert-float-equal
   (/ 453.59237D-3) (units:pound 1D0 units:kilogram))
  (assert-float-equal (/ 16D0) (units:pound 1D0 units:ounce))
  (assert-float-equal 2D3 (units:pound 1D0 units:ton))
  ;; Ton
  (assert-float-equal (/ 907.18474D0) (units:ton 1D0 units:kilogram))
  (assert-float-equal (/ 32D3) (units:ton 1D0 units:ounce))
  (assert-float-equal (/ 2D3) (units:ton 1D0 units:pound)))

;;; Units of temperature

(define-test rankine
  (:tag :us :temperature)
  (assert-float-equal (/ 9D0 5D0) (units:rankine 1D0 units:kelvin)))

;;; Units of speed

(define-test inch/second
  "Test inch/second for known units of speed."
  (:tag :us :speed)
  (assert-float-equal (/ 2.54D-2) (units:inch/second 1D0 units:meter/second))
  (assert-float-equal 12D0 (units:inch/second 1D0 units:foot/second))
  (assert-float-equal 17.6D0 (units:inch/second 1D0 units:mile/hour)))

(define-test foot/second
  "Test foot/second for known units of speed."
  (:tag :us :speed)
  (assert-float-equal (/ 3.048D-1) (units:foot/second 1D0 units:meter/second))
  (assert-float-equal (/ 12D0) (units:foot/second 1D0 units:inch/second))
  (assert-float-equal (/ 5.28D3 3.6D3) (units:foot/second 1D0 units:mile/hour)))

(define-test mile/hour
  "Test mile/hour for known units of speed."
  (:tag :us :speed)
  (assert-float-equal (/ 4.4704D-1) (units:mile/hour 1D0 units:meter/second))
  (assert-float-equal (/ 17.6D0) (units:mile/hour 1D0 units:inch/second))
  (assert-float-equal (/ 3.6D3 5.28D3) (units:mile/hour 1D0 units:foot/second)))
