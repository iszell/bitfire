;	C264
;	1541 drive, multi (Single/Double Clock) resident

BITFIRE_PLATFORM	=	16	;BITFIRE_PLUS4
BF_DRIVE		=	1541
;INSTALLER 		=	0

	!src "../resident/resident-c264-41sb.inc"
	!src "../resident/swaprecv-c264-41sb.inc"
	;!src "../resident/resident-c264-41db.inc"
	;!src "../resident/swaprecv-c264-41db.inc"
	!src "../resident/swaprecv-c264-51db.inc"
bitfire_saveroutine_addr = swapper_saveroutine_addr
	!src "save-resident.asm"
