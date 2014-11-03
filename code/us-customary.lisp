#|

 United States Customary System of Units

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

;;; Units of length

(define-base-unit inch meter 2.540D-2)
(define-base-unit foot meter 3.048D-1)
(define-base-unit yard meter 9.144D-1)
(define-base-unit mile meter 1.609344D3)
(define-base-unit nautical-mile meter 1.852D3)

;;; Units of mass

(define-base-unit ounce kilogram 28.349523125D-3)
(define-base-unit pound kilogram 453.59237D-3)
(define-base-unit ton kilogram 907.18474D0)

;;; Units of temperature

(define-base-unit rankine kelvin (/ 5D0 9D0))

;;; Derived units of speed

(define-derived-unit inch/second meter/second 2.540D-2)
(define-derived-unit foot/second meter/second 3.048D-1)
(define-derived-unit mile/hour meter/second 4.4704D-1)
