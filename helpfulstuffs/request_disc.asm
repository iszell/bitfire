
bitfire_request_disc_
        ;send $fx as command
        ;ldx #$07
        jsr bitfire_send_byte_

!if (BITFIRE_PLATFORM = BITFIRE_C64) {

        ;give floppy time to enter busy mode, and set lines to input
        lda #$3f
        sta $dd02

        ;wait until floppy is idle again
        bit $dd00
        bpl *-3

} else {

		bit link_drive_type
		bmi .case1551

        ;===== 1541

        ;give floppy time to enter busy mode, and set lines to input
        lda #%00001000
        sta $01

        ;wait until floppy is idle again
        bit $01
        bpl *-2

        bmi .waste

        ;===== 1551

.case1551       lda bitfire_51_tcbmbase_addr
		sta .case1551_tia+1		;Set requested TIA address (Unit#8: $FEF0, Unit#9: $FEC0)
		txa
		ldx #0
		dex
		bne *-1
		tax
.case1551_tia	bit $fef0			;<- Previously patched TIA address
		bpl *-3
}

.waste
		rts
