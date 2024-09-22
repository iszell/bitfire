;	C264 (C16 / C116 / plus/4)
;	1541 drive, single clock only

BITFIRE_PLATFORM	=	16	;BITFIRE_PLUS4

!src "../config/config.inc"
!src "../resident/resident-c264-41sc.inc"

BF_DRIVE 		=	1541
BF_PLUS4_BINCOMP	=	0
MULTI_SWAP_INST		=	1		;"Single Clock receiver"

BF_DRV_1541		=	1
BF_DRV_1551		=	0
BF_DRV_1581		=	0
BF_DRV_SD2IEC		=	0

!src "installer.asm"
