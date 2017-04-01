;linking helper macros
!src "../link_macros_acme.inc"

;loader labels
!src "../bitfire/loader_acme.inc"

		* = bitfire_install_
		!bin "../bitfire/installer",,2

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		* = $0900
} else {
		* = $1100
}

.init
		;install loader
		jsr bitfire_install_

		;reset stack shits
		sei

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda #$35
		sta $01
} else {
		sta $ff3f
}
		ldx #$ff
		txs

		;load stage 2, either loaded by bootloader or after turn disk
		jsr link_load_next_comp

		;XXX TODO here it would also be possible to laod a custom link_resident part per side, but would require includes per side and resident part
		jmp $0100
