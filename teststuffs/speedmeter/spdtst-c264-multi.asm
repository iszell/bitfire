
TEST_MODETYPE = 3			;Target: C264 Multi (1541sc / 1541dc / 1551dc)

	!src "../../gen-includes/loader-c264-multi-sc-acme.inc"
	!src "spdtst.asm"
	* = BITFIRE_INSTALLER_ADDR
	!bin "../../gen-binaries/installer-c264-multi-sc.prg",,2
