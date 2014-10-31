#|

 International System of Units

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

(in-package :units)

;;; SI prefixes



;;; SI base units

(define-base-unit
 meter base-length 1D0
 "The meter is the length of the path traveled by light in a vacuum
during a time interval of 1/299,792,458 of a second.")

(define-base-unit
 kilogram base-mass 1D0
 "The kilogram is the unit of mass; it is equal to the mass of the
international prototype of the kilogram.")

(define-base-unit
 seconds base-time 1D0
 "The second is the duration of 9,192,631,770 periods of the radiation
corresponding to the transition between the two hyperfine levels of
the ground state of the cesium-133 atom.")

(define-base-unit
 ampere base-current 1D0
 "The ampere is that constant current which, if maintained in two
straight parallel conductors of infinite length, of negligible
circular cross section, and placed 1 meter apart in vacuum, would
produce between these conductors a force equal to 2E-7 newton per
meter of length.")

(define-base-unit
 kelvin base-temperature 1D0
 "The kelvin, unit of thermodynamic temperature, is the fraction
1/273.16 of the thermodynamic temperature of the triple point of
water.")

(define-base-unit
 mole base-substance 1D0
 "The mole is the amount of substance of a system which contains as
many elementary entites as there are atoms in 0.012 kilogram of carbon
12.")

(define-base-unit
 candela base-luminosity 1D0
 "The candela is the luminous intensity, in a given direction, of a
source that emits monochromatic radiation of frequency 540E+12 hertz
and that has a radiant intensity in that direction of (1/683) watt per
steradian.")

(define-base-unit
 radian nondimensional 1D0
 "The radian is a plane angle in units of m/m.")

;;; Non-SI units accepted for use

(define-base-unit minute seconds 60)
(define-base-unit hour seconds 3600)
(define-base-unit day seconds 86400)

(define-base-unit degree radian (/ pi 180))
