!convtab pet
!cpu 6510

!if (BITFIRE_PLATFORM = BITFIRE_C64) {

screen = $0400
color = $d800
def_charcolor = $1

	* = $801
	!word +,0
	!byte $9e	; sys token
	!text "2061"
+	!byte 0,0,0
} else {

screen = $0c00
color = $0800
def_charcolor = $00

	* = $1001
	!word +,0
	!byte $9e	; sys token
	!text "4109"
+	!byte 0,0,0
}


main:

		ldx	#<headline
		ldy	#>headline		;Headline text
		jsr	bitfire_setbranding

		;ldx	#<swapfilename
		;ldy	#>swapfilename		;Swapfile name
		;lda	#0			;Default: first disk image from swapfile
		ldx	#0
		ldy	#0			;No new swapfile name
		lda	#$ff			;Swapfile not needed
		jsr	bitfire_setswaplist

	jsr bitfire_install_
	sei


!if (BITFIRE_PLATFORM = BITFIRE_C64) {
	lda #$35
	sta $01
} else {
	sta $ff3f
}

	ldx #0
-
	lda screen+$000,x
	sta $2000,x
	lda screen+$100,x
	sta $2100,x
	lda screen+$200,x
	sta $2200,x
	lda screen+$300,x
	sta $2300,x
	inx
	bne -

	jsr clear

	;loading the previously saved screen
	lda #0
	jsr bitfire_loadraw_

	jsr clear


	;loading the save routine
	lda #01
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
	bit link_drive_type
	bpl *+4
	lda #02
}
	jsr bitfire_loadcomp_


	jsr bitfire_save_init
	jsr savestatus

	lda #$00
	sta bitfire_save_data_ptr
	lda #$20
	sta bitfire_save_data_ptr+1

	ldx #1
	ldy #0
	jsr bitfire_save_write_block
	jsr savestatus
	jsr bitfire_save_write_next_block
	jsr savestatus
	jsr bitfire_save_write_next_block
	jsr savestatus
	jsr bitfire_save_write_next_block
	jsr savestatus

	jsr bitfire_save_finish
	jsr savestatus

	jsr clear

	lda #0
	jsr bitfire_loadraw_

!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
	lda	#BITFIRE_STOPMOTOR
	jsr	bitfire_send_byte_
}

		ldx	#7
-		lda	statuses,x
		sta	screen+0,x
		lda	#def_charcolor
		sta	color+0,x
		dex
		bpl	-
		

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#%00000000
		sta	$dc00
-		inc	$d020
		lda	$dc01
		cmp	#%11111111
} else {
		lda	#%00000000
		sta	$fd30
-		inc	$ff19
		sta	$ff08
		lda	$ff08
		cmp	#%11111111
}
		beq	-
		jsr	bitfire_reset_drive_

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#%00110111
		sta	$01
} else {
		sta	$ff3e
}
		jmp	($fffc)

clear:
	ldx #0
	txa
-
	sta screen+$000,x
	sta screen+$100,x
	sta screen+$200,x
	sta screen+$300,x
	inx
	bne -

	rts

;	Save Status code:
;	X <- Status code
;	Cy. <- 0: no new status code, 1: New status code
savestatus	bcc	+		;If no error code, skip
		pha
		txa
		ora	#'0'
		ldx	.sspos
		sta	statuses,x
		tax
		pla
+		inc	.sspos
		rts

.sspos		!byte	0

statuses	!byte	0,0,0,0,0,0,0,0


;swapfilename	!text	"savet-"
;!if (BITFIRE_PLATFORM = BITFIRE_C64) {
;		!text	"c64"
;} else {
;		!text	"c264"
;}
;		!text	".lst",0


headline	!byte	$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
		!text	"<save tester v0.2>"
		!byte	$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40

!src "../../helpfulstuffs/reset_drive.asm"
