;	Overcomplicated, overengineered and oversized Unit selector UI for bitfire
;	CDS: Complex Drive Selector

UNITSEL_RTIME	=	0	;If 1: display rastertime
UNITSEL_DISPALL	=	0	;If 1: display "none" units
UNITSEL_CHINV	=	1	;If 1: display characters inverted
UNITSEL_ANIMSPD	=	0	;If >0: animation speed divided this value

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
_cinv		=	$0314	;IRQ vector
_nminv		=	$0318	;NMI vector
rom_scnkey	=	$ea87	;Keyboard scan
rom_irqreturn	=	$ea81	;IRQ routine end
rasterirq_pos	=	252	;Raster IRQ position
hw_color_bg	=	$d021	;Background color register
hw_color_border	=	$d020	;Border color register
screen_mem	=	$0400	;Screen memory
color_mem	=	$d800	;Color memory
defcol_norm_l	=	$b	;Normal printing color / Low background luminance
defcol_sel_l	=	$0	;Selected printig color / Low background luminance
defcol_norm_h	=	$1	;Normal printing color / High background luminance
defcol_sel_h	=	$f	;Selected printig color / High background luminance
} else {
_cinv		=	$0314	;IRQ vector
rom_scnkey	=	$db11	;Keyboard scan
rom_irqreturn	=	$fcbe	;IRQ routine end
rasterirq_pos	=	210	;Raster IRQ position
hw_color_bg	=	$ff15	;Background color register
hw_color_border	=	$ff19	;Border color register
screen_mem	=	$0c00	;Screen memory
color_mem	=	$0800	;Color memory
defcol_norm_l	=	$71	;Normal printing color / Low background luminance
defcol_sel_l	=	$51	;Selected printig color / Low background luminance
defcol_norm_h	=	$01	;Normal printing color / High background luminance
defcol_sel_h	=	$31	;Selected printig color / High background luminance
}



;	Zero page definitions:
;	'z_unsel' defined earlier
z_chr		=	z_unsel+$00	;Charpointer
z_chrl		=	z_chr+0
z_chrh		=	z_chr+1
z_scm		=	z_unsel+$02	;Source charmem-pointer
z_scml		=	z_scm+0
z_scmh		=	z_scm+1
z_sam		=	z_unsel+$04	;Source attrmem-pointer
z_saml		=	z_sam+0
z_samh		=	z_sam+1
z_dcm		=	z_unsel+$06	;Destination charmem-pointer
z_dcml		=	z_dcm+0
z_dcmh		=	z_dcm+1
z_dam		=	z_unsel+$08	;Destination attrmem-pointer
z_daml		=	z_dam+0
z_damh		=	z_dam+1
z_ypos		=	z_unsel+$0a	;Print position Y
z_xpos		=	z_unsel+$0b	;Print position X
z_lineno	=	z_unsel+$0c	;Line number (counter)
z_printcolor	=	z_unsel+$0d	;Print color code
z_uppercase	=	z_unsel+$0e	;%1x111111: B6 = "uppercase" mask
z_colors	=	z_unsel+$0f	;B×4 Used printing colors
z_duline	=	z_unsel+$13	;Destination Upper line number (12..0)
z_dlline	=	z_unsel+$14	;Destination Lower line number (13..24)
z_suline	=	z_unsel+$15	;Source upper line number
z_slline	=	z_unsel+$16	;Source lower line number
z_ulno		=	z_unsel+$17	;Printable upper line number
z_llno		=	z_unsel+$18	;Printable lower line number



;	Set branding text: copy header to footer and copy new text to header, 40 char:
;	Y:X <- "branding" text address
unitsel_setbranding

		stx	z_chrl
		sty	z_chrh
		ldy	#39
.setbrand_cyc	lda	header_text+1,y
		sta	footer_text+1,y
		lda	(z_chr),y
		sta	header_text+1,y
		dey
		bpl	.setbrand_cyc
		rts



;	Unit selector UI init:
unitsel_init	jsr	interrupt_init

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	hw_color_bg		;BG color
		and	#%00001111
		tax
		lda	.viccoltab,x
		php
		ldx	#defcol_norm_h
		ldy	#defcol_sel_h
		plp
		beq	.setprintcolor
		ldx	#defcol_norm_l
		ldy	#defcol_sel_l
.setprintcolor	stx	z_colors+0
		sty	z_colors+1
		lda	$d018			;Bitmap/Charset address $15/$17 %00010101/%00010111
		sta	_itcharset
		lsr
		lsr
		ror				;Uppercase (0) / Lowercase (1) -> B7
		;and	#%10000000
		lsr
} else {
		lda	$ff15			;BG color
		and	#%00001111
		beq	.bgcolor0
		lda	$ff15
		;and	#%01110000		;Luminance
		and	#%01000000		;Luminance >= 4?
		beq	.bgcolor0
		ldx	#defcol_norm_h
		ldy	#defcol_sel_h
		bne	.setprintcolor
.bgcolor0	ldx	#defcol_norm_l
		ldy	#defcol_sel_l
.setprintcolor	stx	z_colors+0
		sty	z_colors+1
		lda	$ff13			;$D1/$D5 %11010001/%11010101
		sta	_itcharset
		lsr
		lsr
		lsr
		ror				;Uppercase (0) / Lowercase (1) -> B7
		;and	#%10000000
		lsr
}
		ora	#%10111111
		sta	z_uppercase
		lda	#0
		sta	z_ypos
		sta	z_xpos
		jsr	w_setwin2dest		;Set pointers
		ldx	#<footer_text
		ldy	#>footer_text
		jsr	w_print			;Generate footer

		ldx	#<header_text
		ldy	#>header_text
		jsr	w_print			;Generate head
		jmp	w_savescreen		;Save actual screen contents

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
.viccoltab	!byte	0,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1
}

;	Window head / footline
header_text	!byte	ascii_col2
		!byte	$40,$40,$40,$40,$40,$40,$40,$40
		!text	"<bitfire drive selector>"
		!byte	$40,$40,$40,$40,$40,$40,$40,$40
		!byte	ascii_col1,ascii_cr,0

footer_text	!byte	ascii_col2
		!byte	$40,$40,$40,$40,$40,$40,$40,$40
		!byte	$40,$40,$40,$40,$40,$40,$40,$40
		!byte	$40,$40,$40,$40,$40,$40,$40,$40
		!byte	$40,$40,$40,$40,$40,$40,$40,$40
		!byte	$40,$40,$40,$40,$40,$40,$40,$40
		!byte	ascii_col1,ascii_cr,0



;===================
; Unit selector UI:
;===================

unit_selector	sec
		ror	_unitsel_run		;Unitselector called
		jsr	interrupt_enableirq
		lda	_firstscan
		bne	.queryalreadyok
		jsr	unitsel_rescan
.queryalreadyok	jsr	unitsel_screenmain

		ldx	#0
		ldy	#0
		sty	.acceptedkeydrv+0
		sty	_acceptedlines+0
		sty	_accepteddevno+0

.searchdrvs	lda	_selectablesdrv,x	;Selectable drives button ("89ABCDEF" -> "89......")
		beq	.notselect
		sta	.acceptedkeydrv,y
		lda	_selectablelnod,x	;Selectable drives line no in screen
		sta	_acceptedlines,y
		txa
		clc
		adc	#8			;8..15: unit #no
		sta	_accepteddevno,y	;Save unit #no
		iny
		lda	#0
		sta	.acceptedkeydrv,y	;Next position invalid
		sta	_acceptedlines,y
		sta	_accepteddevno,y
.notselect	inx
		cpx	#8
		bne	.searchdrvs
		sty	_acceptedmax		;Save number of acceptable unit no (=0: no unit)

		ldx	#0
		stx	.acceptedindex
		stx	.acceptedpos

		jsr	searchandsetdrive	;Search and set drive

.keycycle	jsr	waitkey
		sta	_downkey
		ldx	#0
.keysearch	lda	.acceptedkeys,x
		beq	.keycycle
		cmp	_downkey
		beq	.keyfound
		inx
		bne	.keysearch		;~BRA
.keyfound	stx	.acceptedindex		;Save "index"
		txa
		asl
		tax
		lda	.acceptkyadr+0,x
		sta	.keycallsub+1
		lda	.acceptkyadr+1,x
		sta	.keycallsub+2
.keycallsub	jsr	$ffff
		jmp	.keycycle


.acceptedkeys	!byte	ascii_down
		!byte	ascii_up
		!byte	ascii_cr
		!byte	'r'
.acceptedkeydrv	!byte	0,0,0,0,0,0,0,0, 0

.acceptedpos	!byte	0
.acceptedindex	!byte	0

.acceptkyadr	!word	.move_down
		!word	.move_up
		!word	.move_return
		!word	.move_rescan
		!word	.move_directsel
		!word	.move_directsel
		!word	.move_directsel
		!word	.move_directsel
		!word	.move_directsel
		!word	.move_directsel
		!word	.move_directsel
		!word	.move_directsel

.move_down	ldx	.acceptedpos
		inx
		lda	.acceptedkeydrv,x
		beq	.movedown_no
.movedown_stncs	jmp	unitsel_setnewpos
.movedown_no	rts

.move_up	ldx	.acceptedpos
		beq	.movedown_no
		dex
		bpl	.movedown_stncs		;BRA

.move_directsel	lda	_acceptedmax
		beq	.movedown_no
		lda	.acceptedindex
		sec
		sbc	#4			;Down, Up, Return, Rescan buttons skip
		tax
		lda	.acceptedkeydrv,x
		beq	.movedown_no
		jsr	unitsel_setnewpos
		lda	_cursorpossv
		cmp	_cursorpos
		bne	*-6			;Wait for IT
		beq	.move_return		;Select

.move_rescan	jsr	unitsel_rescan
.move_newselcyc	pla
		pla
		jmp	unit_selector

.move_return	lda	_acceptedmax
		beq	.movedown_no
		ldx	.acceptedpos
		ldy	_accepteddevno,x	;Read Unit #no of selected
		sty	_selecteddrvno		;Save selected drive Unit #no
!if BITFIRE_UNITSCAN_PRN = 1 {
		lda	_devices-4,y		;Type of selected device
} else {
		lda	_devices-8,y		;Type of selected device
}
		sta	_selecteddrvtyp		;Save selected drive type
!if (BF_DRV_SD2IEC = 1) {
  !if (BITFIRE_FSDV_BITS = 1) {
		cmp	#%00100000+bus_sd2iec	;SD2IEC without VCPU support?
  } else {
		cmp	#%00000000+bus_sd2iec	;SD2IEC without VCPU support?
  }
		bne	.nosd2iecwovcpu
		jmp	unitsel_novcpusupport
.nosd2iecwovcpu
		lda	_selecteddrvtyp
}
		bpl	.move_rescan		;No supported drive?!

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
  !if (BF_DRV_1551 = 1) {
    !if (BITFIRE_1551_ONLY8 != 0) {
		and	#%01111111		;(Maybe unsupported flag cleared)
		cmp	#%01100000+bus_1551	;1551?
		bne	.no1551drive
		ldy	_selecteddrvno		;Unit no
		cpy	#9			;1551 on Unit #9?
		bne	.no1551drive
		jmp	unitsel_no1551onunit9
.no1551drive
    }
  }
}
		and	#%00100000		;Unit limit on serial bus?
		bne	.nounitlimitchk
		lda	_ser_units
		cmp	#1			;Only one device active on the serial bus?
		beq	.nounitlimitchk
		jmp	unitsel_morethanone
.nounitlimitchk
		ldx	#_devicetable_len-1
.savedevscyc	lda	_devices,x
		sta	_devtablesave,x
		dex
		bpl	.savedevscyc
		jsr	unitsel_rescan
		ldx	#_devicetable_len-1
.comparedevtab	lda	_devices,x
		cmp	_devtablesave,x
		bne	.move_newselcyc
		dex
		bpl	.comparedevtab

		jsr	unitsel_screenselected
		ldx	#<.textinitialize
		ldy	#>.textinitialize
		jsr	w_print
		pla
		pla
		lda	#0
		jmp	unitselector_st		;Set + wait

.textinitialize	!text	ascii_cr,"  initialize loader...",ascii_cr,0

searchandsetdrive

		ldx	#0
		lda	_loadfromunit		;Witch unit?
		beq	.noseletedunit
.srccyc		lda	_accepteddevno,x	;Acceptable drive unit# no
		beq	.noseletedunit
		cmp	_loadfromunit
		beq	unitsel_setnewpos
		inx
		cpx	#8
		bne	.srccyc

.noseletedunit	ldx	#0

;	Set new seleted pos on screen:
;	X <- index
unitsel_setnewpos
		lda	_acceptedmax
		beq	+
		stx	.acceptedpos
		lda	_accepteddevno,x	;Acceptable drive unit# no
		beq	+
		sta	_loadfromunit
		lda	_acceptedlines,x
		sta	_cursorpos
+		rts




;	Restore screen:
unitsel_restorescreen
		bit	_unitsel_run		;Unitselector called?
		bmi	+
		rts
+		jsr	w_scnclr		;LineNo = 0: remove window, restore original state
		lda	#0
		jmp	unitselector_st		;Set + wait






;	Rescan units:
unitsel_rescan	jsr	unitsel_screenrescan
		lda	#7
		jsr	unitselector_st		;Set + wait
		lda	#3
		sta	_discoverw
		jmp	unit_scanner


unitselector_st	sta	_setitphase
		lda	#1
		sta	_itworker
		lda	_setitphase
-		cmp	_itworker
		bne	-
		rts


unitsel_morethanone

		ldx	#<.morethanonedev
		ldy	#>.morethanonedev
		sec
		jmp	unitsel_printwarning
.morethanonedev	!text	ascii_cr," more than one device active on",ascii_cr
		!text	" serial bus! please disconnect all",ascii_cr
		!text	" unused devices!",ascii_cr,ascii_cr
		!text	" (",ascii_col2,"remember",ascii_col1,": cbm system's connectors",ascii_cr
		!text	" are not hotpluggable! turn off all",ascii_cr
		!text	" devices and computer first!)",ascii_cr,0

!if (BF_DRV_SD2IEC = 1) {
unitsel_novcpusupport

		ldx	#<.vcpusupportreq
		ldy	#>.vcpusupportreq
		sec
		jmp	unitsel_printwarning
.vcpusupportreq	!text	ascii_cr," this sd2iec firmware does not support",ascii_cr
		!text	" the vcpu"
  !if (BITFIRE_FSDV_40TRK = 1) {
		!text	"-r2"
  }
		!text	" feature. the vcpu feature",ascii_cr
		!text	" is required for operation.",ascii_cr
  !if (BITFIRE_FSDV_40TRK = 1) {
		!text	" (r2 is required for 40 track images.)",ascii_cr
  }
		!text	ascii_cr," for further information, see",ascii_cr,ascii_cr
		!text	"       ",ascii_col2,"bsz.amigaspirit.hu/vcpu",ascii_col1," or",ascii_cr
		!text	"       ",ascii_col2,"bsz.siz.hu/vcpu",ascii_col1,ascii_cr
		!text	ascii_cr," for details.",ascii_cr,0
}

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
  !if (BF_DRV_1551 = 1) {
    !if (BITFIRE_1551_ONLY8 != 0) {
unitsel_no1551onunit9

		ldx	#<.unit9notsupp
		ldy	#>.unit9notsupp
		sec
		jmp	unitsel_printwarning
.unit9notsupp	!text	ascii_cr," 1551 drive does not support on",ascii_cr
		!text	" unit #9. please setup to unit #8.",ascii_cr,0
    }
  }
}



;	Print warning:
;	Y:X <- Warning text
;	Cy  <- 1: print selected device
;	       0: no print
unitsel_printwarning

		txa
		pha
		tya
		pha
		bcs	.printselected
		jsr	w_scnclr
		jmp	*+6
.printselected	jsr	unitsel_screenselected
		pla
		tay
		pla
		tax
		jsr	w_print
		ldx	#<selectscr_resc
		ldy	#>selectscr_resc
		jsr	w_print
		lda	#0			;Next IT phase after up
		jsr	unitselector_st		;Set + wait
		jsr	waitkey
		jsr	unitsel_rescan
		pla
		pla
		jmp	unit_selector



unitsel_screenselected

		jsr	w_scnclr
		ldx	#<.selecteddev
		ldy	#>.selecteddev
		jsr	w_print
		lda	_selecteddrvno
		sec
!if BITFIRE_UNITSCAN_PRN = 1 {
		sbc	#4
} else {
		sbc	#8
}
		tax
		clc
		jsr	unitsel_printoneunit
		jmp	w_linefeed

.selecteddev	!text	ascii_col1,ascii_cr," selected drive:",ascii_cr,0

unitsel_screenrescan

		jsr	w_scnclr
		ldx	#<.rescanscr
		ldy	#>.rescanscr
		jmp	w_print

.rescanscr	!text	ascii_col1,ascii_cr,"       scanning devices: unit #",ascii_cr,0

unitsel_screenmain

		jsr	w_scnclr

		bit	_scanbreak
		bpl	.scancouldrun
		ldx	#<.selectscr_brk
		ldy	#>.selectscr_brk
		clc
		jmp	unitsel_printwarning

.scancouldrun	lda	_all_units
		bne	.anyunits
		ldx	#<.selectscr_nou
		ldy	#>.selectscr_nou
		clc
		jmp	unitsel_printwarning

.anyunits	ldx	#<.selectscr_top
		ldy	#>.selectscr_top
		jsr	w_print
		jsr	unitsel_unittable
		ldx	#<.selectscr_bot
		ldy	#>.selectscr_bot
		jsr	w_print
		ldx	#0
.lettercyc	lda	_selectablesdrv,x
		beq	.noletter
		jsr	w_printchar		;R:ø
.noletter	inx
		cpx	#8
		bne	.lettercyc
		jsr	w_printcont
		lda	#6			;Next IT phase after up
		jmp	unitselector_st		;Set + wait

.selectscr_brk	!text	ascii_col1,ascii_cr,"  scan cannot run due to bus overload!"
		!text	ascii_cr,"  (unpowered devices on the bus?)",ascii_cr,0

.selectscr_nou	!text	ascii_col1,ascii_cr,"   no any device found!",ascii_cr,0

.selectscr_top	!text	ascii_col1,ascii_cr,"    device(s) found:",ascii_cr,ascii_cr,0

.selectscr_bot	!text	ascii_cr," use ",ascii_col2,$1b,"crsr",$1d,ascii_col1
		!text	" and ",ascii_col2,$1b,"return",$1d,ascii_col1
		!text	" to select or",ascii_cr," press key ",ascii_col2,$1b,0
		!text	$1d,ascii_col1," for select drive"
selectscr_resc	!text	ascii_cr," ",ascii_col2,$1b,'r',$1d,ascii_col1,": rescan devices",ascii_cr,0



;	Print discovered units:
unitsel_unittable
		ldx	#0
		stx	.printunit

.cycle		ldx	.printunit
		lda	#0
		sta	_selectables,x			;Delete from acceptable chars table
		sta	_selectablelno,x		;Clear display line no
!if UNITSEL_DISPALL != 1 {
		lda	_devices,x
		and	#%00001111			;DevType
		beq	.nounitdisp
}
		sec					;SEC: "button" printing + pos save required
		jsr	unitsel_printoneunit		;X <- '_devices' table index
		jsr	w_linefeed
.nounitdisp	inc	.printunit
		lda	.printunit
!if BITFIRE_UNITSCAN_PRN = 1 {
		cmp	#12
} else {
		cmp	#8
}
		bne	.cycle
		rts
.printunit	!byte	0


;	Print one unit:
;	X <- Unit index in '_devices' table
;	Cy. <- 0: no print "button" + no save pos, 1: print "button" + save pos
unitsel_printoneunit

		txa
		pha
		lda	#0
		sta	.lineselflag
		inc	z_xpos				;Start line with one space
		bcs	.printableunit
		dec	.lineselflag
		ldx	#<.selectedprint
		ldy	#>.selectedprint
		bne	.notselectabprt			;~BRA
.printableunit	lda	.unitchars,x
		beq	.notselectable
		ldy	_devices,x			;Supported?
		bmi	.charok
!if (BF_DRV_SD2IEC = 1) {
		pha
		tya
		and	#%00001111
		tay
		pla
		cpy	#bus_sd2iec
		beq	.charok
}

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
  !if (BF_DRV_1551 = 1) {
    !if (BITFIRE_1551_ONLY8 != 0) {
		pha
		tya
		and	#%00001111
		tay
		pla
		cpy	#bus_1551
		beq	.charok
    }
  }
}

.notselectable	ldx	#<.threespace
		ldy	#>.threespace
.notselectabprt	jsr	w_print
		jmp	.cont1

.charok		sta	.buttontxt+2
		sta	_selectables,x			;Set to acceptable chars table
		lda	z_lineno
		sta	_selectablelno,x		;Save line number of displayed text
		ldx	#<.buttontxt
		ldy	#>.buttontxt
		jsr	w_print
.cont1		ldx	#<selectscr_unit
		ldy	#>selectscr_unit
		jsr	w_print
		pla
		pha
		asl
		asl				;×4
		tax
		ldy	#3
.unitnoprintc	lda	selectscr_unos,x
		jsr	w_printchar		;R:ø
		inx
		dey
		bpl	.unitnoprintc
		pla
		tax
		lda	_devices,x
		and	#%00001111
		asl
		tax
		lda	.uttxtaddr+1,x
		tay
		lda	.uttxtaddr+0,x
		tax
		jsr	w_print
		bit	.lineselflag
		bpl	.unitprintrdy

.unitprintpadng	ldx	z_xpos
		cpx	#39
		beq	.unitprintend
		lda	#' '
		jsr	w_printchar
		jmp	.unitprintpadng
.unitprintend	lda	#ascii_col1
		jsr	w_printchar
.unitprintrdy	rts

.lineselflag	!byte	0

.unitchars
!if BITFIRE_UNITSCAN_PRN = 1 {
		!byte	0,0,0,0
}
		!text	"89abcdef"

.selectedprint	!text	"   ",ascii_col2,0
.threespace	!text	"   ",0
.buttontxt	;!text	"[?]",0
		!text	ascii_col2,$1b,'?',$1d,ascii_col1,0

.uttxtaddr	!word	selectscr_none
		!word	selectscr_prn
		!word	selectscr_unkn
		!word	selectscr_1541
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		!word	selectscr_1551
} else {
		!word	selectscr_unkn
}
		!word	selectscr_1571
		!word	selectscr_1581
		!word	selectscr_vcpu

selectscr_unit	!text	" unit #"
		!byte	0

selectscr_unos
!if BITFIRE_UNITSCAN_PRN = 1 {
		!text	"4:  "
		!text	"5:  "
		!text	"6:  "
		!text	"7:  "
}
		!text	"8:  "
		!text	"9:  "
		!text	"10: "
		!text	"11: "
		!text	"12: "
		!text	"13: "
		!text	"14: "
		!text	"15: "
		!text	"    "			;<- for IT routine

selectscr_none	!text	"none",0
selectscr_prn	!text	"printer/plotter (maybe)",0
selectscr_unkn	!text	"unknown",0
selectscr_1541	!text	"1541 drive"
!if (BF_DRV_1541 != 1) {
		!text	" (n/s)"
}
		!text	0
selectscr_1551	!text	"1551 drive"
!if (BF_DRV_1551 != 1) {
		!text	" (n/s)"
}
		!text	0
selectscr_1571	!text	"1571 drive"
!if (BF_DRV_1541 != 1) {
		!text	" (n/s)"
} else {
		!text	" (s:1541 mode)"
}
		!text	0
selectscr_1581	!text	"1581 drive"
!if (BF_DRV_1581 != 1) {
		!text	" (n/s)"
}
		!text	0
selectscr_vcpu	!text	"sd2iec drive"
!if (BF_DRV_SD2IEC != 1) {
		!text	" (n/s)"
} else {
		!text	" (s/w vcpu)"
}
		!text	0

_downkey	!byte	0

_selectables
!if BITFIRE_UNITSCAN_PRN = 1 {
		!byte	0,0,0,0
}
_selectablesdrv	!byte	0,0,0,0,0,0,0,0

_selectablelno
!if BITFIRE_UNITSCAN_PRN = 1 {
		!byte	0,0,0,0
}
_selectablelnod	!byte	0,0,0,0,0,0,0,0

_selecteddrvno	!byte	0
_selecteddrvtyp	!byte	0

_devtablesave
!if BITFIRE_UNITSCAN_PRN = 1 {
		!byte	0,0,0,0
}
		!byte	0,0,0,0,0,0,0,0

_acceptedlines	!byte	0,0,0,0,0,0,0,0, 0
_accepteddevno	!byte	0,0,0,0,0,0,0,0, 0
_acceptedmax	!byte	0
_unitsel_run	!byte	0

!src "cds/w_interrupt.asm"
!src "cds/w_textstuffs.asm"

;!align 2047, 0, $55

;---	2K buffer for device selector window (1K attrs, 1K chars)
selectwindow_buffer
	* = * + 2048

;---	2K buffer for saved screen datas (1K attrs, 1K chars)
screensave_buffer
	;* = * + 2048
	* = * + (2048 - 1)	;Small hack:
		!byte	0	;<- Buffers included into the binary
