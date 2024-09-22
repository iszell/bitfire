	!src "../../gen-includes/loader-c264-multi-acme.inc"
	!src "../../gen-includes/saver-c264-41multi-acme.inc"
	;!src "../../gen-includes/saver-c264-51multi-acme.inc"
	!src "savetest.asm"
	* = BITFIRE_INSTALLER_ADDR
	!bin "../../gen-binaries/installer-c264-multi.prg",,2
