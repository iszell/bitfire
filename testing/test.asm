!if TEST_PLUS4 = 1 {
  !src "../bitfire/loader_acme_plus4.inc"
} else {
  !src "../bitfire/loader_acme_c64.inc"	
}

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
	* = $801
	!word +,0
	!byte $9e	; sys token
	!text "2061"
+	!byte 0,0,0
} else {
	* = $1001
	!word +,0
	!byte $9e	; sys token
	!text "4109"
+	!byte 0,0,0
}

main:
	jsr bitfire_install_

	sei

	lda #$55
	jsr clear+2

Bitmap = $2000

!if (BITFIRE_PLATFORM = BITFIRE_C64) {

RamBank = $0000
ColorRam = $0400


	lda #$35
	sta $01

	lda $dd00
	and #%11111100
	ora #<(($c000 - RamBank)/1024/16)
	sta $dd00		; RAM Bank selection
	lda #<( ((ColorRam & $3fff)/1024)*16 + (Bitmap & $3fff)/1024)
	sta $d018		; char screen + charset
	lda #$08
	sta $d016		; multicolor + 40 Cols
	lda $d011
	and #%1011111	; ECM
	ora #$20		; BMM
	sta $d011

	ldx #0
	lda #$01
-   sta ColorRam,x
	sta ColorRam+$100,x
	sta ColorRam+$200,x
	sta ColorRam+$300,x
	inx
	bne -

	;raster irq
	
	lda #$7f
	sta $dc0d
	sta $dd0d
	lda $dc0d
	lda $dd0d

	lda #$01
	sta $d01a

	lda $d011
	and #$7f
	sta $d011

	lda #$32
	sta $d012

	lda #<irq
	sta $fffe
	lda #>irq
	sta $ffff

	cli
	jmp cont

irq:
	pha

	lda $d020
	pha

	lda #$fb
-
	inc $d020
	cmp $d012
	bne -

	pla
	sta $d020

	lsr $d019
	pla
	rti

cont:
				
} else {

ColorRam = $1800

	sta $ff3f

	lda $ff06
	ora #$20
	sta $ff06
	lda $ff12
	and #%11000011
	ora #<(Bitmap/1024)
	sta $ff12
		
	lda #>(ColorRam)
	sta $ff14

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
	jmp cont:
	
irq:
	pha
	
	lda $ff19
	pha

	lda #$cc
-
	inc $ff19
	cmp $ff1d
	bne -
	
	pla
	sta $ff19
	
	inc $ff09
	
	pla
	rti
	
cont:
}
	
	lda #0
	jsr bitfire_loadraw_
	ldx #1
	jsr chksum
	
	jsr link_load_next_raw
	ldx #2
	jsr chksum
	
	jsr link_load_next_raw
	ldx #3
	jsr chksum
	
	jsr link_load_next_raw
	ldx #4
	jsr chksum
	
	jsr link_load_next_raw
	ldx #5
	jsr chksum
	
loop:
	jsr clear

	jsr test_raw
	jsr swap_receivers
	jsr test_lz_next
	jsr swap_receivers
	jsr test_raw_next
	jsr swap_receivers
	jsr test_lz_next
	jsr swap_receivers
	jsr test_raw_next
	jsr swap_receivers

	jmp loop

swap_receivers:

!ifdef bitfire_plus4_swap_receiver {
	jsr bitfire_plus4_swap_receiver
	lda $ff19
	eor #$ee
	sta $ff19
}
	rts

test_raw_next:
	lda #BITFIRE_LOAD_NEXT
	jmp test_raw+2

test_raw:
	lda #0
	jsr bitfire_loadraw_
	ldx #1
	jsr compare

	jsr link_load_next_raw
	ldx #2
	jsr compare

	jsr link_load_next_raw
	ldx #3
	jsr compare

	jsr link_load_next_raw
	ldx #4
	jsr compare

	jsr link_load_next_raw
	ldx #5
	jsr compare
	rts

test_lz_next:
!if BITFIRE_DECOMP = 1 {
	
	jsr link_load_next_comp
	ldx #1
	jsr compare
	
	jsr link_load_next_comp
	ldx #2
	jsr compare
	
	jsr link_load_next_comp
	ldx #3
	jsr compare
	
	jsr link_load_next_comp
	ldx #4
	jsr compare
	
	jsr link_load_next_comp
	ldx #5
	jsr compare

}
	rts


clear:
	lda #$f0
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


chksum:
p = $fe
	lda #0
	sta p
	lda #$20
	sta p+1
k:
	ldy #0
	clc
	lda (p),y
	iny
	adc (p),y
	iny
	eor (p),y
	iny
	adc (p),y
	iny
	eor (p),y
	iny
	adc (p),y
	iny
	eor (p),y
	iny
	adc (p),y
	pha
	lda p
	sta .q
	lda p+1
	ora #$40
	sta .q+1
	pla
.q = *+1
	sta $6000,x
	
	clc
	lda p
	adc #8
	sta p
	tay
	lda p+1
	adc #0
	sta p+1
	cpy #$40
	bne k
	cmp #$3f
	bne k
	rts

compare:
	txa
	pha
	ldx #0
	jsr chksum
	pla
	tay

	lda #0
	sta p
	lda #$60
	sta p+1
.cl
    ldx #0
	lda (p,x)
	eor (p),y
	beq +

	lda p
	sta .cq
	lda p+1
	and #$1f
	lsr
	ror .cq
	lsr
	ror .cq
	lsr
	ror .cq
!if BITFIRE_PLATFORM = BITFIRE_C64 {
	ora #>ColorRam
} else {
	ora	#(>ColorRam)+4
}
	sta .cq+1
	lda #$02
.cq = *+1
	sta $1800,x

+	clc
	lda p
	adc #8
	sta p
	tax
	lda p+1
	adc #0
	sta p+1
	cpx #$40
	bne .cl
	cmp #$7f
	bne .cl
	rts


	* = BITFIRE_INSTALLER_ADDR

!if BITFIRE_PLATFORM = BITFIRE_PLUS4 {

!bin "../bitfire/installer_plus4_41dc_swap.prg",,2

} else {

!bin "../bitfire/installer_c64.prg",,2
	
}