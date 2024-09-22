	!src "../../gen-includes/loader-c64-multi-acme.inc"
	!src "test.asm"
	* = BITFIRE_INSTALLER_ADDR
	!bin "../../gen-binaries/installer-c64-multi.prg",,2
