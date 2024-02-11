;	Device selector, text-a-ware stuffs

ascii_col1	=	1
ascii_col2	=	2
ascii_col3	=	3
ascii_col4	=	4
ascii_cr	=	13
ascii_down	=	17
ascii_right	=	29
ascii_up	=	145
ascii_left	=	157

windowlineno	=	20	;Selector screen buffer Y size
windowcenter	=	11	;11/12 char. line of screen: window center



;	×40 multiplier:
;	A <- Multiplied value
;	X <- ZP offset (ZP+X=Lo,ZP+(X+1)=Hi)
; R : A
m_mul40		asl
		rol	$01,x
		asl
		rol	$01,x
		asl
		rol	$01,x			;×8
		sta	$00,x
		lda	$01,x
		and	#%00000111
		sta	$01,x
		pha
		lda	$00,x
		asl
		rol	$01,x
		asl
		rol	$01,x			;×32
		;clc				;(Not required, Cy. already 0)
		adc	$00,x			;Add ×8 lo BYTE
		sta	$00,x
		pla
		adc	$01,x			;Add ×8 hi BYTE
		sta	$01,x
		rts

;;	+40 (adder):
;;	X <- ZP offset (ZP+X=Lo,ZP+(X+1)=Hi)
;; R : ø
;m_add40		pha
;		clc
;		lda	$00,x
;		adc	#40
;		sta	$00,x
;		bcc	*+4
;		inc	$01,x
;		pla
;		rts

;;	-40 (subtracter):
;;	X <- ZP offset (ZP+X=Lo,ZP+(X+1)=Hi)
;; R : ø
;m_sub40		pha
;		sec
;		lda	$00,x
;		sbc	#40
;		sta	$00,x
;		bcs	*+4
;		dec	$01,x
;		pla
;		rts



;	Save original screen contents
w_savescreen	ldx	#0
.copy		lda	color_mem+$000,x
		sta	screensave_buffer+$000,x
		lda	color_mem+$100,x
		sta	screensave_buffer+$100,x
		lda	color_mem+$200,x
		sta	screensave_buffer+$200,x
		lda	color_mem+$300,x
		sta	screensave_buffer+$300,x
		lda	screen_mem+$000,x
		sta	screensave_buffer+$400,x
		lda	screen_mem+$100,x
		sta	screensave_buffer+$500,x
		lda	screen_mem+$200,x
		sta	screensave_buffer+$600,x
		lda	screen_mem+$300,x
		sta	screensave_buffer+$700,x
		inx
		bne	.copy
		rts

;	Set destination pointers to "selectwindow" area:
;	A <- Line number
; R : A,X
w_setwin2dest	ldx	#z_dcm
		jsr	m_mul40			;R:A A reg.×40 -> z_dcm
		clc
		lda	z_dcml
		adc	#<selectwindow_buffer
		sta	z_dcml
		sta	z_daml
		lda	z_dcmh
		adc	#>selectwindow_buffer
		sta	z_damh
		adc	#>(1024)
		sta	z_dcmh
		rts

;	Clear screen (buffer):
w_scnclr	jsr	.scnclr_settop
		lda	z_colors+0
		sta	z_printcolor
		ldx	#windowlineno-1
.clrcyc2	ldy	#39
.clrcyc1
!if UNITSEL_CHINV = 1 {
		lda	#%10000000+' '
} else {
		lda	#' '
}
		sta	(z_dcm),y
		lda	z_printcolor
		sta	(z_dam),y
		dey
		bpl	.clrcyc1
		jsr	w_linefeed
		dex
		bpl	.clrcyc2
		lda	#0
		sta	z_lineno
		sta	z_xpos			;Start: left
.scnclr_settop	lda	#2
		sta	z_ypos			;Start: top-left corner
		jmp	w_setwin2dest		;R:A,X Set destination pointers

;	LF: set next line:
; R : ø
w_linefeed	pha
		txa
		pha
		inc	z_ypos
		lda	z_ypos
		jsr	w_setwin2dest		;R:A,X
		inc	z_lineno		;~LF
		lda	#0
		sta	z_xpos			;~CR
		pla
		tax
		pla
		rts

;	Print character:
; R : ø
w_printchar	cmp	#ascii_cr
		beq	w_linefeed
		bcc	.printcommand
		sty	.yrest+1
		ldy	z_xpos
		cmp	#$41
		bcc	.okay
		and	z_uppercase
.okay
!if UNITSEL_CHINV = 1 {
		eor	#%10000000
}
		sta	(z_dcm),y
		pha
		lda	z_printcolor
		sta	(z_dam),y
		pla
		iny
		sty	z_xpos
.yrest		ldy	#0
		rts

.printcommand	stx	.xrest+1
		pha
		tax
		dex				;color "codes" 1..4 -> 0..3
		lda	z_colors,x
		sta	z_printcolor
		pla
.xrest		ldx	#$ff			;<- Modified
		rts

;	Print line:
;	Y:X <- source text, zero terminated
;	Y   -> source text pointer index, points to after $00
; R : A,X,Y

w_print		stx	z_chrl
		sty	z_chrh
		ldy	#0

;	Continue line printing:
;	Y <- source text pointer index
w_printcont	lda	(z_chr),y
		beq	.textend
		jsr	w_printchar		;R:ø
		iny
		bne	w_printcont		;~BRA
.textend	iny
		rts

