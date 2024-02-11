;	C264
;	1551 drive, multi resident

BITFIRE_PLATFORM	=	16	;BITFIRE_PLUS4
BF_DRIVE		=	1551
;INSTALLER 		=	0

	!src "../resident/resident-c264-51db.inc"
	!src "../resident/swaprecv-c264-51db.inc"
bitfire_saveroutine_addr = swapper_saveroutine_addr
	!src "save-resident.asm"
