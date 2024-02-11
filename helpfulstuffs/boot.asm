!if PLATFORM=64 {
 	!src "../gen-includes/loader-c64-multi-acme.inc"

	* = $801
	!word +,0
	!byte $9e	; sys token
	!text "2061"
+	!byte 0,0,0
} else {
 	!src "../gen-includes/loader-c264-multi-acme.inc"

	* = $1001
	!word +,0
	!byte $9e	; sys token
	!text "4109"
+	!byte 0,0,0
}

 	jsr bitfire_install_

	sei

!if PLATFORM=64 {
	lda #$35
	sta $01
} else {
	sta $ff3f
}

	ldx #$ff
	txs

	lda #>(START-1)                 ;jump to START
	pha
	lda #<(START-1)
	pha

!if LOAD=2 {                            ;load and decomp 2nd file
	lda #>(link_load_next_comp-1)
	pha
	lda #<(link_load_next_comp-1)
	pha
}

	jmp link_load_next_comp         ;load and decomp 1st file

	;rts will load and jump to the address pushed on the stack...


	* = BITFIRE_INSTALLER_ADDR

!if PLATFORM=64 {
	!bin "../gen-binaries/installer-c64-multi.prg",,2
} else {
	!bin "../gen-binaries/installer-c264-multi.prg",,2
}
