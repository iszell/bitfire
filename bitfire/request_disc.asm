!ifndef BITFIRE_IS_INCLUDED {
!src "loader_acme.inc"
}
bitfire_request_disc_
		;send $fx as command
		;ldx #$07
		jsr bitfire_send_byte_

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		;bit $dd00
		;bmi *-3

		;give floppy time to enter busy mode, and set lines to input
		lda #$3f
		sta $dd02

		;wait until floppy is idle again
                bit $dd00
                bpl *-3
.waste
		rts
} else {
  !if (BF_DRIVE = 1541) {				;===== 1541
		;bit $01
		;bmi *-2

		;give floppy time to enter busy mode, and set lines to input
		lda #%11001000
		sta $01

		;wait until floppy is idle again
                bit $01
                bpl *-2
  } else {						;===== 1551
		txa
		ldx	#0
		dex
		bne	*-1
		tax
		bit	$fef0
		bpl	*-3
  }
.waste
		rts
}
