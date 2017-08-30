!if TEST_PLUS4 = 0 {
	!src "../bitfire/loader_acme_c64.inc"
bfsave=$400
}
!if TEST_PLUS4 = 1 {
	!src "../bitfire/loader_acme_plus4_1541.inc"
bfsave=$480
}
!if TEST_PLUS4 = 2 {
	!src "../bitfire/loader_acme_plus4_1551.inc"
bfsave=$400
}
!if TEST_PLUS4 = 3 {
	!src "../bitfire/loader_acme_plus4_multi.inc"
bfsave=$480
}

!if (BITFIRE_PLATFORM = BITFIRE_C64) {

screen = $0400

	* = $801
	!word +,0
	!byte $9e	; sys token
	!text "2061"
+	!byte 0,0,0
} else {

screen = $0c00

	* = $1001
	!word +,0
	!byte $9e	; sys token
	!text "4109"
+	!byte 0,0,0
}


main:
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
	bit bitfire_drive_type
	bpl *+4
	lda #2
}
	jsr bitfire_loadcomp_


	jsr bfsave


	lda #$00
	sta $d0
	lda #$20
	sta $d1

	lda #$d0
	ldx #1
	ldy #0
	jsr bfsave+6



	inc $d1

	lda #$d0
	ldx #1
	ldy #4
	jsr bfsave+6

	inc $d1

	lda #$d0
	ldx #1
	ldy #8
	jsr bfsave+6

	inc $d1

	lda #$d0
	ldx #1
	ldy #12
	jsr bfsave+6

	jsr bfsave+3

	jsr clear

	lda #0
	jsr bitfire_loadraw_

	jmp *

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

	* = BITFIRE_INSTALLER_ADDR

!if TEST_PLUS4 = 0 {
	!bin "../bitfire/installer_c64.prg",,2
}
!if TEST_PLUS4 = 1 {
	!bin "../bitfire/installer_plus4_1541sc_swap.prg",,2
}
!if TEST_PLUS4 = 2 {
	!bin "../bitfire/installer_plus4_1551.prg",,2
}
!if TEST_PLUS4 = 3 {
	!bin "../bitfire/installer_plus4_multi.prg",,2
}
