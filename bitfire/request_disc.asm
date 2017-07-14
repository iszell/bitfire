
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
        lda #%11001000
        sta $01

        ;wait until floppy is idle again
        bit $01
        bpl *-2

        bmi .waste

.case1551       
        ;===== 1551

        txa
        ldx #0
        dex
        bne *-1
        tax
        bit $fef0
        bpl *-3

}

.waste
        rts
