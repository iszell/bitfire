;	C264 (C16 / C116 / plus/4)
;	1551 drive, double clock only

BITFIRE_PLATFORM	=	16	;BITFIRE_PLUS4

!src "../config/config.inc"
!src "../resident/resident-c264-51dc.inc"

BF_DRIVE		=	1551
BF_PLUS4_BINCOMP	=	0
;BITFIRE_PLUS4_MODE	=	BITFIRE_PLUS4_1551
MULTI_SWAP_INST		=	0

BF_DRV_1541		=	0
BF_DRV_1551		=	1
BF_DRV_1581		=	0
BF_DRV_SD2IEC		=	0

!src "installer.asm"
