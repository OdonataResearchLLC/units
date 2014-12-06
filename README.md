## units

*units* is a Common Lisp library that provides simple units
conversion. The basis units for the library are the [International
System of Units (SI)][SI].

### Features

* Written in portable Common Lisp
* Can be used simply as a set of conversion functions
* Can be integrated into objects to track and check units in equations
  (sort of)
* Extensible with user defined units
* Performs simple dimensional analysis to create conversion factors
  for derived units.

### How to use units

At this preliminary stage, there is no documentation for the *units*
library.

1. Load using [ASDF][] : `(asdf:load-system :units)`.
2. Convert a unit, for example 2.3 feet to meters:
   `(meter 2.3 foot) => 0.70104`
3. Define a base unit: `(define-base-unit inch meter 2.540D-2)`
4. Define a derived unit:
   `(define-derived-unit inch^2 (* meter meter) 6.4516D-4)`
   or simply `(define-derived-unit inch^2 (* inch inch))`.

Defining a unit binds a global variable of the unit name to the unit
instance and creates a conversion function for that unit. Simple
dimensional analysis of conversions is performed by the conversion
function to verify that the target unit is compatible with the source
unit.

*NOTE:* There is an small inconsistency in the naming convention for
the units. In general, units are named in the singular, except for
`seconds`. The `seconds` unit had to be plural to avoid a name clash
with `cl:second`.

## Version 0.1.0 Features

* SI base units `meter`, `kilogram`, `seconds`, `ampere`, `kelvin`,
  `mole`, `candela`
* Angle units `radian` and `degree`
* Time units `minute`, `hour`, and `day`
* US Customary units `inch`, `foot`, `yard`, `mile`, `nautical-mile`,
  `ounce`, `pound`, `ton`, `rankine`

### TODO

* Implement SI prefixes
* Expand the units defined in the library
* Document the library in the project wiki
* Expand dimensional analysis capabilities

[ASDF]: <http://common-lisp.net/project/asdf/> "ASDF"
[SI]: <http://physics.nist.gov/cuu/Units/bibliography.html> "(SI)"
