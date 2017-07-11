
!src "../bitfire/loader_acme_plus4_multi.inc"

	* = $1001
	!word +,0
	!byte $9e	; sys token
	!text "4109"
+	!byte 0,0,0

Bitmap = $2000
ColorRam = $1800

main:

	jsr bitfire_install_

	sei
	sta $ff3f

	jsr clear

	ldx #0
-
	lda #$70
	sta ColorRam,x
	sta ColorRam+$100,x
	sta ColorRam+$200,x
	sta ColorRam+$300,x
	lda #$01
	sta ColorRam+$400,x
	sta ColorRam+$500,x
	sta ColorRam+$600,x
	sta ColorRam+$700,x
	inx
	bne -

	lda $ff06
	ora #$20
	sta $ff06
	lda $ff12
	and #%11000011
	ora #<(Bitmap/1024)
	sta $ff12
	lda #>(ColorRam)
	sta $ff14

	lda #$02
	sta $ff0a
	lda #$03
	sta $ff0b
	inc $ff09
	lda #<irq
	sta $fffe
	lda #>irq
	sta $ffff

	cli
	jmp loop

irq:
	pha
	inc $ff19
	lda #$cc
-
	cmp $ff1d
	bne -

	dec $ff19

	inc $ff09

	pla
	rti

loop:
	lda #0
	jsr bitfire_loadraw_
	jsr link_load_next_raw
	jsr link_load_next_raw
	jsr link_load_next_raw
	jsr link_load_next_raw

!if BITFIRE_DECOMP = 1 {
	jsr link_load_next_comp
	jsr link_load_next_comp
	jsr link_load_next_comp
	jsr link_load_next_comp
	jsr link_load_next_comp
}

	jsr clear

	jmp loop

clear:
	lda #$00
	ldy #$20
	sty l+2
	ldx #0
l:
	sta Bitmap,x
	inx
	bne l
	inc l+2
	dey
	bne l
	rts


	* = BITFIRE_INSTALLER_ADDR
!bin "../bitfire/installer_plus4_multi.prg",,2	;including without loading address
