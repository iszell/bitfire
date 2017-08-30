!if TEST_PLUS4 = 0 {
	!src "../bitfire/loader_acme_c64.inc"	
}
!if TEST_PLUS4 = 1 {
	!src "../bitfire/loader_acme_plus4_1541.inc"
}
!if TEST_PLUS4 = 2 {
	!src "../bitfire/loader_acme_plus4_1551.inc"
}
!if TEST_PLUS4 = 3 {
	!src "../bitfire/loader_acme_plus4_multi.inc"
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


!if (BITFIRE_PLATFORM = BITFIRE_C64) {
	ldx #0
-
	lda $0400,x
	sta $2400,x
	lda $0500,x
	sta $2500,x
	lda $0600,x
	sta $2600,x
	lda $0700,x
	sta $2700,x
	inx
	bne -

	lda #1
	jsr bitfire_loadraw_

	lda #0
	jsr bitfire_loadcomp_
}

	jsr $400

	lda #$00
	sta $d0
	lda #$24
	sta $d1

	lda #$d0
	ldx #1
	ldy #12
	jsr $406

	inc $d1

	lda #$d0
	ldx #1
	ldy #16
	jsr $406

	inc $d1

	lda #$d0
	ldx #1
	ldy #20
	jsr $406

	inc $d1

	lda #$d0
	ldx #1
	ldy #1
	jsr $406

	jsr $403


	ldx #0
	txa
-
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
	sta $0800,x
	sta $0900,x
	sta $0a00,x
	sta $0b00,x
}
	inx
	bne - 

	jsr link_load_next_comp

	jmp *

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
