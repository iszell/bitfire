	!src "../../gen-includes/loader-c64-multi-acme.inc"
	!src "../../gen-includes/saver-c64-41-acme.inc"
	!src "savetest.asm"
	* = BITFIRE_INSTALLER_ADDR
	!bin "../../gen-binaries/installer-c64-multi.prg",,2
