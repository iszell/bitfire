;	Direct Sector handler routines...

!cpu 6510

!src "../config/config.inc"

!if (BF_DRIVE = 1541) {				;===== 1541
	!src "save-drive-41.inc"
} else {					;===== 1551
	!src "save-drive-51.inc"
}

;!if INSTALLER {
;
;.sectorrouts_installer
;
;		ldx #0
;.cl
;		lda .sectrouts_start,x
;		sta bitfire_saveroutine_addr,x
;		inx
;		bne +
;		inc .cl + 2
;		inc .cl + 5
;+
;		cpx #<(.sectrouts_end-.sectrouts_start)
;		bne .cl
;		lda .cl + 2
;		cmp #>.sectrouts_end
;		bne .cl
;		
;		rts
;
;} ; INSTALLER
;
;.sectrouts_start

!ifdef BITFIRE_SAVE_ADDR {
	* = BITFIRE_SAVE_ADDR
} else {
	* = bitfire_saveroutine_addr
}

;bitfire_save_addr = *

_sector_limit_17:
;sec  17  18  19  20
!byte 31, 25, 18, 18

bitfire_save_write_next_block

	lda .block_sector+1
	clc
	adc #BITFIRE_CONFIG_INTERLEAVE
	tay
	lax .block_track+1
	cpy #17                      ;less than 17 is always ok
	bcc .upd_ts
	cpy #21                      ;more than 20 is never ok
	bcs +
	cmp _sector_limit_17-17,y
	bcc .upd_ts                  ;it's still ok on this track
+	iny
!if (BITFIRE_CONFIG_INTERLEAVE&(BITFIRE_CONFIG_INTERLEAVE-1)) = 0 {  ;interleave is 2^n
	tya
	and #BITFIRE_CONFIG_INTERLEAVE-1
	tay
} else {
	tya
	sec
-	tay
	sbc #BITFIRE_CONFIG_INTERLEAVE
	bcs -
	tya
}
	bne .upd_ts                  ;we continue on same track
	inx                          ;next track
        cpx #18
        bne .upd_ts
        inx
.upd_ts
	inc .block_pointer+2


;	Sector Write routine:
;	X <- Track number (1..35, 36..40)
;	Y <- Sector number (0..16/17/18/20)
bitfire_save_write_block

		stx	.block_track+1
		sty	.block_sector+1
		lda	#<drv_writep_start_offs+_save_drivecodes
		ldx	#>drv_writep_start_offs+_save_drivecodes
		ldy	#drv_writep_size - 1
		jsr	upload_to_drv		;Upload sector transfer and preparation routine
bitfire_save_track = * + 1
.block_track	lda	#0			;Upload sector datas
		jsr	sendbyte_todrv		;Send BYTE to drive
bitfire_save_sector = * + 1
.block_sector	lda	#0
		jsr	sendbyte_todrv		;Send BYTE to drive
		ldy	#$ff
bitfire_save_data_ptr = * + 1
.block_pointer	lda	$bab,y			;Self-modified: read datas from block
		jsr	sendbyte_todrv		;Send BYTE to drive
		dey
		cpy	#$ff
		bne	.block_pointer
		jsr	wait_drv_idle

		lda	#<drv_writed_start_offs+_save_drivecodes
		ldx	#>drv_writed_start_offs+_save_drivecodes
		ldy	#drv_writed_size - 1
		jsr	upload_to_drv		;Upload sector writer routine
		ldx	#0
		beq	wait_drv_idle		;shorter jmp


;	Exit routine: return to normal BF mode:
bitfire_save_finish

		lda	#<drv_exit_start_offs+_save_drivecodes
		ldx	#>drv_exit_start_offs+_save_drivecodes
		ldy	#drv_exit_size - 1
		bne	.dsr_download		;shorter jmp

;	Upload code to drive: (Max size: 256 BYTEs!)
upload_to_drv	sta	.upload_addr + 1
		stx	.upload_addr + 2	;Set address
.upload_addr	lda	$ffff,y			;Read BYTE
		jsr	sendbyte_todrv		;Send BYTE to drive
		dey
		cpy	#$ff
		bne	.upload_addr
		rts

;	Init routine: Turn on drive motor (if required):
bitfire_save_init

		lda	#<drv_init_start_offs+_save_drivecodes
		ldx	#>drv_init_start_offs+_save_drivecodes
		ldy	#drv_init_size - 1
.dsr_download	jsr	upload_to_drv		;Z=1
		ldx	#0
		;beq	wait_drv_idle		;shorter jmp

;	Wait drive idle:
wait_drv_idle
		clc
		pha
		jsr	.waste12
		jsr	.waste24
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#$3f
		sta	$dd02			;Release bus
		bit	$dd00
		bpl	*-3
		bit	$dd00
		bvs	+
  !if (BITFIRE_SAVE_LEGACYMODE != 0) {			;Legacy mode: own status-recv protocol
		lda	#$37
		sta	$dd02
		jsr	.waste24
		lda	$dd00
		asl
		rol
		rol
		and	#%00000011
		tax
		inx
		lda	#$3f
		sta	$dd02
  } else {						;New mode: use BF's BYTE-recv routine
		lda	#$60			;RTS op.code
		jsr	bitfire_41_bitfire_ack	;Recv. one BYTE from drive
		lsr
		lsr
		and	#%00000011
		tax
		inx
  }
		sec
} else {
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	#%00001000		;Release bus
		sta	$01
		bit	$01
		bpl	*-2
		bit	$01
		bvs	+
    !if (BITFIRE_SAVE_LEGACYMODE != 0) {		;Legacy mode: own status-recv protocol
		lda	#%00001100		;ATN drive
		sta	$01
		jsr	.waste12
		jsr	.waste24
		lda	$01
		asl
		rol
		rol
		and	#%00000011
		tax
		inx
		lda	#%00001000		;ATN released
		sta	$01
    } else {						;New mode: use BF's BYTE-recv routine
		lda	#$60			;RTS op.code
		jsr	bitfire_41_bitfire_ack	;Recv. one BYTE from drive
		lsr
		lsr
		and	#%00000011
		tax
		inx
    }
		sec
  } else {						;===== 1551
		ldx	bitfire_51_tcbmbase_addr	;$FEF0/$FEC0: X -> $F0/$C0
		lda	$fe00,x
		bpl	*-3
		lda	$fe00,x
		and	#%01000000
		bne	.xto0
		inc	$fe04,x
		jsr	.waste24
		lda	$fe00,x
		and	#%00000011
		dec	$fe04,x
		tax
		inx
		sec
		top				;NOP $xxxx, skip "LDX #$00" instruction
.xto0		ldx	#0
  }
}
+		pla

!if (BF_DRIVE = 1551) {					;===== 1551
.waste24
		jsr *+3
}

.waste12
		rts

!if (BF_DRIVE = 1541) {					;===== 1541
.waste24
		jsr *+3
		rts
}



;	Send BYTE to drive:
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
sendbyte_todrv
		ldx	#$3f
		stx	$dd02			;Release bus
		bit	$dd00
		bpl	*-3
		jmp	bitfire_send_byte_
} else {
  !if (BF_DRIVE = 1541) {				;===== 1541
sendbyte_todrv
		ldx	#%00001000		;Release bus
		stx	$01
		bit	$01
		bpl	*-2
		jmp	bitfire_send_byte_
  } else {						;===== 1551
sendbyte_todrv = bitfire_send_byte_
  }
}



;	Precompiled drivecodes here:
_save_drivecodes
!if (BF_DRIVE = 1541) {				;===== 1541
	!bin "save-drive-41"
} else {					;===== 1551
	!bin "save-drive-51"
}
