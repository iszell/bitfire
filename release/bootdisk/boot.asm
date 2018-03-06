!if PLATFORM=16 {
 	!src "../bitfire/loader_acme_plus4_multi.inc"
}
!if PLATFORM=64 {
 	!src "../bitfire/loader_acme_c64.inc"
}

	* = $1100

 	jsr bitfire_install_

	sei

!if PLATFORM=16 {
	sta $ff3f
}
!if PLATFORM=64 {
	lda #$35
	sta $01
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

!if PLATFORM=16 {
	!bin "../bitfire/installer_plus4_multi.prg",,2
}
!if PLATFORM=64 {
	!bin "../bitfire/installer_c64.prg",,2
}
