#|

 Unit Tests for International System of Units

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

 References
 [NIST] "Guide for the Use of the International System of Units (SI)",
        NIST Special Publication 811, 2008 Edition

|#

(in-package :test-units)

;;; SI base units

(define-test angle-units
  (:tag :base-units :si)
  ;; Radians
  (assert-float-equal pi (units:radian 180 units:degree))
  (assert-float-equal (/ pi 2) (units:radian 90 units:degree))
  ;; Degrees
  (assert-float-equal 180D0 (units:degree pi units:radian))
  (assert-float-equal 90D0 (units:degree (/ pi 2) units:radian)))

(define-test time-units
  (:tag :base-units :si)
  ;; Seconds
  (assert-float-equal 30D0 (units:seconds 5D-1 units:minute))
  (assert-float-equal 1.8D3 (units:seconds 5D-1 units:hour))
  (assert-float-equal 4.32D4 (units:seconds 5D-1 units:day))
  ;; Minutes
  (assert-float-equal 2D0 (units:minute 120D0 units:seconds))
  (assert-float-equal 30D0 (units:minute 5D-1 units:hour))
  (assert-float-equal 60D0 (units:minute (/ 24D0) units:day))
  ;; Hours
  (assert-float-equal 2D0 (units:hour 7.2D3 units:seconds))
  (assert-float-equal 2D0 (units:hour 1.2D2 units:minute))
  (assert-float-equal 12D0 (units:hour 5D-1 units:day))
  ;; Days
  (assert-float-equal 5D-1 (units:day 4.32D4 units:seconds))
  (assert-float-equal 2D0 (units:day 2.88D3 units:minute))
  (assert-float-equal 2D0 (units:day 48D0 units:hour)))
