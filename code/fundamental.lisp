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

(in-package :units)

;;; Angle conversions

(defun radians (degrees)
  "Return the angle in radians."
  (* degrees (/ pi 180D0)))

(defun degrees (radians)
  "Return the angle in degrees."
  (* radians (/ 180D0 pi)))

;;; Expedient conversion functions
;;; FIXME : Remove when real units system is implemented.

(defun meters (inches)
  "Convert the length from inches to meters."
  (float (* 2.54D-2 inches) inches))

(defun inches (meters)
  "Convert the length from meters to inches."
  (float (/ meters 2.54D-2) meters))

(defun newtons (pounds-force)
  "Convert the force from pounds-force to newtons."
  (float (* 4.448222D0 pounds-force) pounds-force))

(defun pounds-force (newtons)
  "Convert the force from newtons to pounds-force."
  (float (/ newtons 4.448222D0) newtons))

(defun pascals (psi)
  "Return the pressure in Pascals."
  (float (/ psi 1.45D-4) psi))

(defun psi (pascals)
  "Return the pressure in PSI."
  (float (* 1.45D-4 pascals) pascals))
