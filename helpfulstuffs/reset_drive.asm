
bitfire_reset_drive_
!zone {
	;upload data
		ldy	#.upload_size - 1
-		lda	.upload_start,y
		jsr	.sendbyte_todrv
		dey
		bpl	-

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#$3f
		sta	$dd02			;Release bus
		rts
} else {
		bit	link_drive_type
		bmi	+
	;===== 1541
		lda	#%00001000		;Release bus
		sta	$01
		rts
	;===== 1551
+		ldy	bitfire_51_tcbmbase_addr
		lda	#%11111111
		sta	$fe03,y			;TIA DDRA B7..0 output
		lda	#%00000000
		sta	$fe04,y			;TIA DDRB B10 input
		sta	$fe00,y			;TIA PORTA = $00 (default: output and $00)
		lda	#%01000000
		sta	$fe05,y			;TIA DDRC B6 output (DAV)
		sta	$fe02,y			;TIA PORTC B6 hi (DAV inactive)
		rts
}

	;send one byte to drive:
.sendbyte_todrv
	;check if drive is idle
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		ldx	#$3f
		stx	$dd02			;Release bus
		bit	$dd00
		bpl	*-3
} else {
		bit	link_drive_type
		bmi	+
        ;===== 1541
		ldx	#%00001000		;Release bus
		stx	$01
		bit	$01
		bpl	*-2
		bmi	++

+       ;===== 1551
		ldx	bitfire_51_tcbmbase_addr
		stx	.case1551_tia+1		;Set requested TIA address (Unit#8: $FEF0, Unit#9: $FEC0)
.case1551_tia	bit	$fef0			;<- patched TIA address
		bpl	*-3
++
}
	;send byte
		jmp	bitfire_send_byte_

.upload_start
!pseudopc $0108 {
.code_start
        ;code to be uploaded goes here
		jmp	($fffc)			;This "code" working with all type of drives
.code_end
}
.code_size = .code_end - .code_start;

		!byte	>(.code_size - 1), <(.code_size - 1)
		!byte	>(.code_end - 1),  <(.code_end - 1)
		!byte	BITFIRE_UPLOAD
.upload_end
.upload_size = .upload_end - .upload_start;
}
