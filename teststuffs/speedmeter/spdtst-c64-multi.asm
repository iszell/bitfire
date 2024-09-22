
TEST_MODETYPE = 0			;Target: C64

	!src "../../gen-includes/loader-c64-multi-acme.inc"
	!src "spdtst.asm"
	* = BITFIRE_INSTALLER_ADDR
	!bin "../../gen-binaries/installer-c64-multi.prg",,2
