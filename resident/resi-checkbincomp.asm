;===========================================================
;	Binary compatible residents
;	Compatibility check
;
;	  Simple ACME (0.97?) trick:
;
;	If you define a symbol and then define it again
;	later with the same name but a different value,
;	you get an error message when compiling. ("Symbol
;	already defined.") But if the value also matches,
;	no errors will occur.
;
;	This source inserts the definitions of the three
;	"binary-compatible" versions from the resident
;	codes. If symbols with the same name do not match,
;	the compiling aborted with an error.
;===========================================================
	!cpu 6510

	!src "resident-c264-41sb.incc"
	!src "resident-c264-41db.incc"
	!src "resident-c264-51db.incc"

	* = 0
	!word	bitfire_resident_size
;===========================================================
