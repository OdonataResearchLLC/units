#|

 Units

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

(in-package :cl-user)

(defpackage :units
  (:use :common-lisp)
  ;; Fundamental operations
  (:export :convert)
  ;; Units object
  (:export :define-unit
           :name
           :definition
           :reference-units
           :conversion-factor
           :unitp)
  ;; SI base units
  (:export :meter :kilogram :seconds :ampere :kelvin :mole :candela)
  ;; Non-SI base units
  (:export :radian :degree :minute :hour :day)
  ;; SI speed
  (:export :meter/second)
  ;; US customary length
  (:export :inch :foot :yard :mile :nautical-mile)
  ;; US customary mass
  (:export :ounce :pound :ton)
  ;; US customary temperature
  (:export :rankine)
  ;; US customary speed
  (:export :inch/second :foot/second :mile/hour))
