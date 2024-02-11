;	C264 (C16 / C116 / plus/4)
;	1541/1551/SD2IEC drive, multi installer (double clock only)

BITFIRE_PLATFORM	=	16	;BITFIRE_PLUS4

!src "../config/config.inc"
;!src "../resident/resident-c264-41sb.inc"
;!src "../resident/resident-c264-41db.incc"
;!src "../resident/resident-c264-51db.incc"
;!src "../resident/swaprecv-c264-41sb.inc"
;!src "../resident/swaprecv-c264-41db.inc"
;!src "../resident/swaprecv-c264-51db.inc"

;bitfire_plus4_swap_receiver = swapper_plus4_swap_receiver
;bitfire_saveroutine_addr = swapper_saveroutine_addr

BF_DRIVE 		=	1541
BF_PLUS4_BINCOMP	=	1
MULTI_SWAP_INST		=	0

BF_DRV_1541		=	1
BF_DRV_1551		=	1
BF_DRV_1581		=	0
BF_DRV_SD2IEC		=	1

!src "installer.asm"
