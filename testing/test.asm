
!src "../bitfire/loader_acme.inc"

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

Bitmap = $2000

!if (BITFIRE_PLATFORM = BITFIRE_C64) {

RamBank = $0000
ColorRam = $0400


	lda #$35
	sta $01

	lda $dd00
	and #%11111100
	ora #<(($c000 - RamBank)/1024/16)
	sta $dd00	; RAM Bank selection
	lda #<( ((ColorRam & $3fff)/1024)*16 + (Bitmap & $3fff)/1024)
	sta $d018	; char screen + charset
	lda #$08
	sta $d016	; multicolor + 40 Cols
	lda $d011
	and #%1011111	; ECM
	ora #$20    	; BMM
	sta $d011

	ldx #0
	lda #$01
-	sta ColorRam,x
	sta ColorRam+$100,x
	sta ColorRam+$200,x
	sta ColorRam+$300,x
	inx
	bne -

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
-   lda #$70
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

}

loop:
	ldy #$20
	sty l+2
	lda #$f0
l:	sta Bitmap,x
	inx
	bne l
	inc l+2
	dey
	bne l

	lda #$00
	jsr bitfire_loadraw_
	jsr link_load_next_raw
	jmp loop

	* = BITFIRE_INSTALLER_ADDR
!bin "../bitfire/installer",,2

