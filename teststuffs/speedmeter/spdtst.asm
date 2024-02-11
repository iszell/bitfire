!convtab pet
!cpu 6510

z_lineno	=	$fd
z_ptr		=	$fe


!if (BITFIRE_PLATFORM = BITFIRE_C64) {
  !if (BITFIRE_FRAMEWORK_MUSIC_NMI != 0) { !error "IRQ required, not NMI!" }

ColorRam = $d800
CharRam = $0400
CharColor = $01

	* = $801
	!word	+,0
	!byte	$9e	; sys token
	!text	"2061"
+	!byte	0,0,0
} else {

ColorRam = $0800
CharRam = $0c00
CharColor = $00

	* = $1001
	!word	+,0
	!byte	$9e	; sys token
	!text	"4109"
+	!byte	0,0,0
}

main:		ldx	#<headline
		ldy	#>headline		;Headline text
		jsr	bitfire_setbranding

		;ldx	#<swapfilename
		;ldy	#>swapfilename		;Swapfile name
		;lda	#0			;Default: first disk image from swapfile
		ldx	#0
		ldy	#0			;No new swapfile name
		lda	#$ff			;Swapfile not needed
		jsr	bitfire_setswaplist

		jsr	bitfire_install_

		sei

		ldy	#3
		ldx	#0
.scnclrcyc	lda	#' '
.charramwr	sta	CharRam,x
		lda	#CharColor
.colorramwr	sta	ColorRam,x
		inx
		bne	.scnclrcyc
		inc	.charramwr+2
		inc	.colorramwr+2
		dey
		bpl	.scnclrcyc

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#$35
		sta	$01
	;raster irq
		lda	#$7f
		sta	$dc0d
		sta	$dd0d
		lda	$dc0d
		lda	$dd0d

		lda	#$01
		sta	$d01a

		lda	$d011
		and	#$7f
		sta	$d011

		lda	#$f8
		sta	$d012
		lsr	$d019
} else {
		sta	$ff3f
		lda	#$02
		sta	$ff0a
		lda	#$d0
		sta	$ff0b
		inc	$ff09
}

		lda	#<loaddisplayer
		sta	link_music_addr+0
		lda	#>loaddisplayer
		sta	link_music_addr+1

		lda	#<link_player
		sta	$fffe
		lda	#>link_player
		sta	$ffff
		cli

.loop		lda	#0
		jsr	bitfire_loadraw_
		lda	#0
		sta	z_lineno
		jsr	ldwithtimer_x5

!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		lda	#BITFIRE_STOPMOTOR
		jsr	bitfire_send_byte_
}

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		jsr	swaprecv

		lda	#0
		jsr	bitfire_loadraw_
		jsr	ldwithtimer_x5

		jsr	swaprecv

  !if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		lda	#BITFIRE_STOPMOTOR
		jsr	bitfire_send_byte_
  }
}
		jmp	.loop

ldwithtimer_x5:	lda	#4
		sta	.ldcount
.ldcycle	lda	z_lineno
		jsr	loadwithtimer
		inc	z_lineno
		dec	.ldcount
		bpl	.ldcycle
		rts
.ldcount	!byte	0

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
swaprecv:	jsr	bitfire_plus4_swap_receiver
		lda	$ff19
		eor	#$ee
		sta	$ff19
		rts
}

loaddisplayer:
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		inc	$d020
} else {
		inc	$ff19
}
		jsr	getloadaddrhi			;"lda bitfire_load_addr_lo+1" ;Actual load address Hi
		beq	.exit
		sec
		sbc	#$20				;Load files from $2000..
		tax
		lda	#$23				;"#"
		sta	CharRam,x
.exit
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		dec	$d020
} else {
		dec	$ff19
}
		rts

;	Load next with time measurement:
loadwithtimer:	sta	.lineno+1
		ldx	#$ff
		ldy	#$ff
		jsr	displayvalues			;"????"
		lda	#0
		jsr	setloadaddrhi			;"sta bitfire_load_addr_lo+1" ;Actual load address Hi = 0: invalid address, no any print
		ldx	#0
		lda	#' '
-		sta	CharRam,x
		inx
		bne	-
		;ldx	#0
		sei
		stx	link_frame_count+0
		stx	link_frame_count+1		;Timer = ø
		cli
		jsr	link_load_next_raw		;Load next file
		sei
		ldx	link_frame_count+0
		ldy	link_frame_count+1		;Read actual timer
.lineno		lda	#$ff				;<- Self-modified
		jsr	displayvalues
		cli
		rts

;	Display measured value:
;	A   <- Line number
;	Y:X <- Value
displayvalues:	sta	.textend+1
		stx	.numlo+1
		sty	.numhi+1
		tax
		lda	.lineposislo,x
		sta	z_ptr+0
		lda	.lineposishi,x
		sta	z_ptr+1
		ldy	#0
.textcopy	lda	.script,y
		beq	.textend
		sta	(z_ptr),y
		iny
		bne	.textcopy
.textend	lda	#$ff				;Self-Modified
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		asl
}
		tax
		lda	.flnos,x
		ldy	#6
		sta	(z_ptr),y
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		iny
		lda	.flnos+1,x
		sta	(z_ptr),y
}
		tya
		clc
		adc	#21
		tay
.numhi		lda	#$ff				;Self-modified
		cmp	#$ff				;Invalid..?
		beq	.numinvalid
		jsr	.printnum
.numlo		lda	#$ff				;Self-modified
.printnum	pha
		lsr
		lsr
		lsr
		lsr
		jsr	.printlo
		pla
.printlo	and	#%00001111
		tax
		lda	.hexnums,x
		sta	(z_ptr),y
		iny
		rts

.numinvalid	lda	#$3f				;"?"
		ldx	#3
.ninvcyc	sta	(z_ptr),y
		iny
		dex
		bpl	.ninvcyc
		rts

.lineposislo	!byte	<CharRam+(8*40)
		!byte	<CharRam+(9*40)
		!byte	<CharRam+(10*40)
		!byte	<CharRam+(11*40)
		!byte	<CharRam+(12*40)
		!byte	<CharRam+(14*40)
		!byte	<CharRam+(15*40)
		!byte	<CharRam+(16*40)
		!byte	<CharRam+(17*40)
		!byte	<CharRam+(18*40)
.lineposishi	!byte	>CharRam+(8*40)
		!byte	>CharRam+(9*40)
		!byte	>CharRam+(10*40)
		!byte	>CharRam+(11*40)
		!byte	>CharRam+(12*40)
		!byte	>CharRam+(14*40)
		!byte	>CharRam+(15*40)
		!byte	>CharRam+(16*40)
		!byte	>CharRam+(17*40)
		!byte	>CharRam+(18*40)

!convtab scr {
.script
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		!text	"load # : counted frames = $     "
} else {
		!text	"load #  : counted frames = $     "
}
		!byte	0
.flnos
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		!text	"12345"
} else {
		!text	"s1s2s3s4s5d1d2d3d4d5"
}
.hexnums	!text	"0123456789abcdef"
}

;	1541/1551: get load address Hi:
getloadaddrhi:
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		bit	link_chip_types
		bmi	+
}
		lda	bitfire_41_load_addr_lo+1
		rts
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
+		lda	bitfire_51_load_addr_lo+1
		rts
}

;	1541/1551: set load address Hi (for interrupt routine):
setloadaddrhi:
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		bit	link_chip_types
		bmi	+
}
		sta	bitfire_41_load_addr_lo+1
		rts
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
+		sta	bitfire_51_load_addr_lo+1
		rts
}

;swapfilename	!text	"speedt-"
;!if (BITFIRE_PLATFORM = BITFIRE_C64) {
;		!text	"c64"
;} else {
;		!text	"c264"
;}
;		!text	".lst",0

headline	!byte	$40,$40,$40,$40,$40,$40,$40,$40
		!text	"<load speed tester v0.1>"
		!byte	$40,$40,$40,$40,$40,$40,$40,$40
