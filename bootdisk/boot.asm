 	!src "../bitfire/loader_acme_plus4_multi.inc"

	* = $1100
 
 	jsr bitfire_install_

	sei
	sta $ff3f
	ldx #$ff
	txs
	
	lda #>(START-1)                 ;jump to START
	pha
	lda #<(START-1)
	pha
	
!if LOAD=2 {                        ;load and decomp 2nd file
	lda #>(link_load_next_comp-1)
	pha
	lda #<(link_load_next_comp-1)
	pha
}
	jmp link_load_next_comp         ;load and decomp 1st file

	* = BITFIRE_INSTALLER_ADDR

	!bin "../bitfire/installer_plus4_multi.prg",,2
