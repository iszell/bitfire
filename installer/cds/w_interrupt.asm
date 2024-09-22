;	Device selector, interrupt stuffs

;	Raster interrupt and IT routine init:
interrupt_init	lda	#0
		sta	_itworker		;No function
		sta	_itcounter
		sta	_headsize
		sta	z_suline
		sta	z_slline

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#<nmi_routine
		sta	_nminv+0
		lda	#>nmi_routine
		sta	_nminv+1		;Set up temporary NMI routine

		lda	#%01111111
		sta	$dc0d			;CIA #1 IRQ disable
		sta	$dd0d			;CIA #2 NMI disable
		lda	$dc0d			;Clear pending IRQs
		lda	$dd0d			;Clear "pending" NMIs

		lda	$d011
		and	#%01111111
		ora	#((rasterirq_pos >> 1) & %10000000)
		sta	$d011
		lda	#<rasterirq_pos
		sta	$d012			;Raster IRQ position set
		lda	#%00000001
		sta	$d01a			;Enable raster interrupt
} else {
		lda	#<rasterirq_pos
		sta	$ff0b
		lda	#>rasterirq_pos | %00000010
		sta	$ff0a			;Enable raster interrupt, disable all other TED IRQ
}
		lda	#<irq_routine
		sta	_cinv+0
		lda	#>irq_routine
		sta	_cinv+1			;Set up interrupt routine
		rts

;	Temporary NMI routine / C64:
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
nmi_routine	rti
}

;	Interrupt enable:
interrupt_enableirq
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#%00001111
		sta	$d019			;Clear pending IRQs
} else {
		lda	#%11111111
		sta	$ff09
}
		cli
		rts



;====================
; Interrupt routine:
;====================

irq_routine
!if (UNITSEL_RTIME = 1) {
		inc	hw_color_border
}
		jsr	rom_scnkey	;KERNAL Keyboard scan
		lda	_itcharset
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		sta	$d018
} else {
		sta	$ff13
}
!if (UNITSEL_ANIMSPD > 0) {
		jsr	anim_speed
		bcc	.noanim
}
		lda	_itworker
		asl
		tax
		lda	.itrouts+0,x
		sta	.itwork+1
		lda	.itrouts+1,x
		sta	.itwork+2
.itwork		jsr	$ffff		;<- self-modified call
.noanim

		lda	_itcounter
		beq	*+5
		dec	_itcounter

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	$dc0d		;Clear interrupt flags / CIA #1
		lda	#%00001111
		sta	$d019		;Clear interrupt flags / VIC-II
} else {
		lda	#%11111111
		sta	$ff09		;Clear interrupt flags / TED
}
!if (UNITSEL_RTIME = 1) {
		dec	hw_color_border
}
		jmp	rom_irqreturn

;	IT work addresses:
.itrouts	!word	itwork_none		;0: none/ready
		!word	itwork_headup		;1: Head/footer up
		!word	itwork_screendowninit	;2: Screen "remover" init
		!word	itwork_screendown	;3: Screen "remover"
		!word	itwork_screenupinit	;4: Screen displayer init
		!word	itwork_screenup		;5: Screen displayer
		!word	itwork_cursor		;6: "Cursor" mover
		!word	itwork_printunit	;7: Print actual unit number to screen
		!word	itwork_none		;8: ?
		!word	itwork_headrm		;9: Head/Footer remover

!if (UNITSEL_ANIMSPD > 0) {
anim_speed	inc	_animspd
		lda	_animspd
		cmp	#UNITSEL_ANIMSPD
		bne	.no
		lda	#0
		sta	_animspd
		sec
		rts
.no		clc
		rts
}

;===================
; IT work routines:
;===================

;	0: No function
itwork_none	lda	#0
		sta	_itworker
		rts

;	1: Animating up head/footer
itwork_headup	lda	_headsize
		cmp	#20
		bne	.headrun
		inc	_itworker
		rts
.headrun	inc	_headsize
		;inc	_headsize
		lda	#windowcenter
		jsr	setdestptrs
		lda	#20
		sec
		sbc	_headsize
		tay
		lda	_headsize
		asl
		sta	_headxsize

		ldx	#0
.hdftc		lda	selectwindow_buffer+$0400+40,x
		sta	(z_dcm),y
		lda	selectwindow_buffer+$0000+40,x
		sta	(z_dam),y
		tya
		pha
		clc
		adc	#40
		tay
		lda	selectwindow_buffer+$0400+0,x
		sta	(z_dcm),y
		lda	selectwindow_buffer+$0000+0,x
		sta	(z_dam),y
		pla
		tay
		iny
		inx
		cpx	_headxsize
		bne	.hdftc
		rts

;	2: Screen "remover" init:
itwork_screendowninit
		inc	_itworker
		lda	z_suline
		ora	z_slline
		bne	itwork_screendown
		inc	_itworker		;Remove not necessary, go to up
		jmp	itwork_screenupinit

;	3: Screen "remover":
itwork_screendown
		jsr	.removeup
		ldx	#1			;Line1: Head
		ldy	z_duline
		jsr	setsrcdestptrs
		jsr	copyoneline
		jsr	.removedown
		ldx	#0			;Line0: Footer
		ldy	z_dlline
		jsr	setsrcdestptrs
		jsr	copyoneline
		lda	z_duline
		cmp	#windowcenter
		bne	.ru_notready
		lda	z_dlline
		cmp	#windowcenter+1
		bne	.ru_notready
		inc	_itworker		;Next: up
.ru_notready	rts

.removeup	ldy	z_duline
		cpy	#windowcenter
		beq	.removeup_rdy
		ldx	z_duline
		jsr	setsrcdestptrs_r
		jsr	copyoneline
		inc	z_duline
.removeup_rdy	rts

.removedown	ldy	z_dlline
		cpy	#windowcenter+1
		beq	.removedn_rdy
		ldx	z_dlline
		jsr	setsrcdestptrs_r
		jsr	copyoneline
		dec	z_dlline
.removedn_rdy	rts

;	4: Animating up window (init):
itwork_screenupinit
		lda	z_lineno		;(New) Window number of lines
		bne	.windowup_setup
		lda	#0
		sta	_headsize
		lda	#40
		sta	_headxsize
		lda	#9			;Head/Footer remover
		sta	_itworker
		rts
.windowup_setup	ldx	#windowcenter
		stx	z_duline		;Dest. upper line
		inx
		stx	z_dlline		;Dest. lower line
		lda	z_lineno		;Number of char.lines
		lsr
		sta	z_suline		;Src. upper line
		inc	z_suline
		inc	z_suline		;+2 (Skip head+footer)
		sta	z_slline
		inc	z_slline		;Src. lower line +1
		inc	z_slline
		inc	z_slline		;+2 (Skip head+footer)
		sta	z_ulno			;Number of upper lines
	inc	z_ulno
		adc	#0			;Add cy.
		sta	z_llno			;Number of lower lines
		lda	#$ff
		sta	_cursorpos		;Invalid position
		sta	_cursorpossv		;-||- save
		inc	_itworker		;Set next phase

;	5: Animating up window
itwork_screenup	jsr	.printup
		ldx	#1			;Line1: Head
		ldy	z_duline
		jsr	setsrcdestptrs
		jsr	copyoneline
		jsr	.printdown
		ldx	#0			;Line0: Footer
		ldy	z_dlline
		jsr	setsrcdestptrs
		jsr	copyoneline
		lda	z_ulno
		ora	z_llno			;Any lines remaining?
		bne	.su_notready
		lda	_setitphase
		sta	_itworker		;Set screen ready
.su_notready	rts

.printup	lda	z_ulno
		beq	.printup_rdy
		dec	z_ulno
		ldx	z_suline
		ldy	z_duline
		jsr	setsrcdestptrs
		jsr	copyoneline
		dec	z_suline
		dec	z_duline
.printup_rdy	rts

.printdown	lda	z_llno
		beq	.printdown_rdy
		dec	z_llno
		ldx	z_slline
		ldy	z_dlline
		jsr	setsrcdestptrs
		jsr	copyoneline
		inc	z_slline
		inc	z_dlline
.printdown_rdy	rts

;	6: "Cursor" mover
itwork_cursor	lda	_cursorpossv
		cmp	_cursorpos
		bne	*+3
		rts
		lda	_cursorpossv
		bmi	.notremove		;If saved position is invalid, no displayed cursor, no remove from screen
		ldy	z_colors+0
		jsr	.cursorpush
.notremove	lda	_cursorpos
		sta	_cursorpossv
		bmi	.notpush
		ldy	z_colors+1
.cursorpush	clc
		adc	z_duline
		adc	#1			;"UNIT #..." line position
		jsr	setdestptrs
		tya
		ldy	#4
.crpushcyc	sta	(z_dam),y
		iny
		cpy	#39
		bne	.crpushcyc
.notpush	rts

;	7: Print actual unit number to screen:
itwork_printunit
		lda	z_fa
		sec
!if BITFIRE_UNITSCAN_PRN = 1 {
		sbc	#4
} else {
		sbc	#8
}
		asl
		asl			;×4
		tax
		lda	selectscr_unos,x
!if UNITSEL_CHINV = 1 {
		eor	#%10000000
}
		sta	screen_mem+(windowcenter*40)+31
		lda	selectscr_unos+1,x
		cmp	#':'
		bne	*+4
		lda	#' '
!if UNITSEL_CHINV = 1 {
		eor	#%10000000
}
		sta	screen_mem+(windowcenter*40)+32
		rts

;	9: Head/Footer remover:
itwork_headrm	ldx	#windowcenter
		ldy	#windowcenter
		jsr	setsrcdestptrs_r		;Set source/dest pointers (original screen / screen)
		jsr	copyoneline			;Restore screen / head position
		ldy	#40
		jsr	copyoneline_idx			;Restore screen / footer position
		lda	_headsize			;0..19
		cmp	#20
		bne	*+5
		jmp	itwork_none
		tay
		asl
		tax
.hdftrmc	lda	selectwindow_buffer+$0400+40,x
		sta	(z_dcm),y
		lda	selectwindow_buffer+$0000+40,x
		sta	(z_dam),y
		tya
		pha
		clc
		adc	#40
		tay
		lda	selectwindow_buffer+$0400+0,x
		sta	(z_dcm),y
		lda	selectwindow_buffer+$0000+0,x
		sta	(z_dam),y
		pla
		tay
		iny
		inx
		cpy	_headxsize
		bne	.hdftrmc
		inc	_headsize
		dec	_headxsize
		rts

;	Set source/dest pointers (window -> screen)
;	X <- Source line no (window buffer)
;	Y <- Destination line no (screen mem)
setsrcdestptrs	txa
		ldx	#z_scm
		jsr	m_mul40
		clc
		lda	z_scml
		adc	#<selectwindow_buffer
		sta	z_scml
		sta	z_saml
		lda	z_scmh
		adc	#>selectwindow_buffer
		sta	z_samh
		adc	#>(1024)
		sta	z_scmh
		tya

;	Set dest pointers:
;	A <- Destination line no
setdestptrs	ldx	#z_dcm
		jsr	m_mul40
		lda	z_dcml
		sta	z_daml
		clc
		lda	z_dcmh
		adc	#>color_mem
		sta	z_damh
		;clc
		lda	z_dcmh
		adc	#>screen_mem
		sta	z_dcmh
		rts

;	Set source/dest pointers (original screen -> screen)
;	X <- Source line no (original screen)
;	Y <- Destination line no (screen mem)
setsrcdestptrs_r
		txa
		ldx	#z_scm
		jsr	m_mul40
		clc
		lda	z_scml
		adc	#<screensave_buffer
		sta	z_scml
		sta	z_saml
		lda	z_scmh
		adc	#>screensave_buffer
		sta	z_samh
		adc	#>(1024)
		sta	z_scmh
		tya
		jmp	setdestptrs

;	Copy one line (char + attr):
copyoneline	ldy	#0
;	Copy one line (with offset) (char + attr)
;	Y <- Offset
copyoneline_idx	ldx	#39
.print_cp	lda	(z_scm),y
		sta	(z_dcm),y
		lda	(z_sam),y
		sta	(z_dam),y
		iny
		dex
		bpl	.print_cp
		rts



;	Wait a key:
waitkey		lda	#0
		sta	z_ndx
.waitkeycyc	lda	z_ndx
		beq	.waitkeycyc
		lda	_keyd+0
		rts

!if (UNITSEL_ANIMSPD > 0) {
_animspd	!byte	0
}
_itcharset	!byte	0
_itworker	!byte	0
_headsize	!byte	0
_headxsize	!byte	0
_cursorpos	!byte	0
_cursorpossv	!byte	0
_setitphase	!byte	0
_discoverw	!byte	0
_itcounter	!byte	0
