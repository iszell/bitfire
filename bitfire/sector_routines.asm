;	Direct Sector handler routines...

!cpu 6510

!macro sector_routines INSTALLER, BF_DRIVE {

!zone dirsecthandlers {

!if INSTALLER {

.sectorrouts_installer

		ldx #0
.cl
		lda .sectrouts_start,x
		sta BITFIRE_SAVE_ADDR,x
		inx
		bne +
		inc .cl + 2
		inc .cl + 5
+
		cpx #<(.sectrouts_end-.sectrouts_start)
		bne .cl
		lda .cl + 2
		cmp #>.sectrouts_end
		bne .cl
		
		rts

} ; INSTALLER

.sectrouts_start


!pseudopc BITFIRE_SAVE_ADDR {


  !if (BF_DRIVE = 1541) {				;===== 1541
.IDLE		=	drivecode_41_IDLE
.BUSY		=	drivecode_41_BUSY
.BLOCK_READY	=	drivecode_41_BLOCK_RDY
.VIA2_LED_ON	=	drivecode_41_VIA2LED_ON
.VIA2_MOTOR_ON	=	drivecode_41_VIA2MTR_ON
.bin2ser	=	drivecode_41_bin2ser
.to_track	=	drivecode_41_to_track
.sector		=	drivecode_41_sector
.filenum	=	drivecode_41_filenum
.dirsect	=	drivecode_41_dirsect
.head_lo	=	drivecode_41_head_lo
.preamble_lo	=	drivecode_41_preamb_lo
.preamble_hi	=	drivecode_41_preamb_hi
.pull_command	=	drivecode_41_pullcomm
.motor_on	=	drivecode_41_motoron
.get_byte	=	drivecode_41_getbyte
.seek		=	drivecode_41_seek
.drivecode_launch =	drivecode_41_launch
.dos_bin2gcr	=	$f77f
  } else {						;===== 1551
.IDLE		=	drivecode_51_IDLE
.BUSY		=	drivecode_51_BUSY
.BLOCK_READY	=	drivecode_51_BLOCK_RDY
.TCPU_LED_ON	=	drivecode_51_TCPULED_ON
.to_track	=	drivecode_51_to_track
.sector		=	drivecode_51_sector
.filenum	=	drivecode_51_filenum
.dirsect	=	drivecode_51_dirsect
.head_lo	=	drivecode_51_head_lo
.preamble_lo	=	drivecode_51_preamb_lo
.preamble_hi	=	drivecode_51_preamb_hi
.pull_command	=	drivecode_51_pullcomm
.motor_on	=	drivecode_51_motoron
.get_byte	=	drivecode_51_getbyte
.seek		=	drivecode_51_seek
.drivecode_launch =	drivecode_51_launch
.dos_bin2gcr	=	$f8bf
  }

.bin2gcr_rdp	=	.preamble_lo + 0	;Bin-to-GCR Read pointer
.bin2gcr_wrp	=	.preamble_lo + 2	;Bin-to-GCR Write pointer
.bin2gcr_cnt	=	.preamble_lo + 4	;Bin-to-GCR converter counter
.bin2gcr_buff	=	.preamble_hi		;Bin-to-GCR converter temp buffer
.writecnt	=	.preamble_hi + 0
.verifycnt	=	.preamble_hi + 1
.sectorcnt	=	.preamble_hi + 2

.mem_blockbinb	=	$06fc			;Temporary bin. block storage area
.mem_blockgcrb	=	$06bb			;Temporary GCR block storage area
.mem_headerbinb	=	$06b3			;Temporary bin. header storage area
.mem_headergcrb	=	$06b1			;Temporary GCR header storage area


.sector_limit_17:
;sec  17  18  19  20
!byte 31, 25, 18, 18

bitfire_save_write_next_block_offs = * - BITFIRE_SAVE_ADDR
	lda .block_sector+1
	clc
	adc #BITFIRE_CONFIG_INTERLEAVE
	tay
	lax .block_track+1
	cpy #17                      ;less than 17 is always ok
	bcc .upd_ts
	cpy #21                      ;more than 20 is never ok
	bcs +
	cmp .sector_limit_17-17,y
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
bitfire_save_write_block_offs = * - BITFIRE_SAVE_ADDR
.dsr_sectorwrt
		stx	.block_track+1
		sty	.block_sector+1
		lda	#<.drv_writep_start
		ldx	#>.drv_writep_start
		ldy	#.drv_writep_size - 1
		jsr	.upload_to_drv		;Upload sector transfer and preparation routine
bitfire_save_track_offs = * + 1 - BITFIRE_SAVE_ADDR
.block_track	lda	#0			;Upload sector datas
		jsr	.sendbyte_todrv		;Send BYTE to drive
bitfire_save_sector_offs = * + 1 - BITFIRE_SAVE_ADDR
.block_sector	lda	#0
		jsr	.sendbyte_todrv		;Send BYTE to drive
		ldy	#$ff
bitfire_save_data_ptr_offs = * + 1 - BITFIRE_SAVE_ADDR
.block_pointer	lda	$bab,y			;Self-modified: read datas from block
		jsr	.sendbyte_todrv		;Send BYTE to drive
		dey
		cpy	#$ff
		bne	.block_pointer
		jsr	.wait_drv_idle

		lda	#<.drv_writed_start
		ldx	#>.drv_writed_start
		ldy	#.drv_writed_size - 1
		jsr	.upload_to_drv		;Upload sector writer routine
		ldx	#0
		beq	.wait_drv_idle		;shorter jmp


;	Exit routine: return to normal BF mode:
bitfire_save_finish_offs = * - BITFIRE_SAVE_ADDR
.dsr_exit	lda	#<.drv_exit_start
		ldx	#>.drv_exit_start
		ldy	#.drv_exit_size - 1
		bne	.dsr_download		;shorter jmp

;	Upload code to drive: (Max size: 256 BYTEs!)
.upload_to_drv	sta	.upload_addr + 1
		stx	.upload_addr + 2	;Set address
.upload_addr	lda	$ffff,y			;Read BYTE
		jsr	.sendbyte_todrv		;Send BYTE to drive
		dey
		cpy	#$ff
		bne	.upload_addr
		rts

;	Init routine: Turn on drive motor (if required):
bitfire_save_init_offs = * - BITFIRE_SAVE_ADDR
.dsr_init	lda	#<.drv_init_start
		ldx	#>.drv_init_start
		ldy	#.drv_init_size - 1
.dsr_download	jsr	.upload_to_drv
		ldx	#0
		;beq	.wait_drv_idle		;shorter jmp

;	Wait drive idle:
.wait_drv_idle
		clc
		pha
		jsr	++
		jsr	++
		jsr	++
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#$3f
		sta	$dd02			;Release bus
		bit	$dd00
		bpl	*-3
		bit	$dd00
		bvs	+
		lda	#$37
		sta	$dd02
		jsr	++
		jsr	++
		lda	$dd00
		asl
		rol
		rol
		and	#%00000011
		tax
		inx
		lda	#$3f
		sta	$dd02
		sec
} else {
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	#%11001000		;Release bus
		sta	$01
		bit	$01
		bpl	*-2
		bit	$01
		bvs	+
		lda	#%11001100		;ATN drive
		sta	$01
		jsr	++
		jsr	++
		jsr	++
		lda	$01
		asl
		rol
		rol
		and	#%00000011
		tax
		inx
		lda	#%11001000		;ATN released
		sta	$01
		sec
  } else {						;===== 1551
		bit	$fef0
		bpl	*-3
		bit	$fef0
		bvs	+
		inc	$fef4
		jsr	++
		jsr	++
		lda	$fef0
		and	#%00000011
		tax
		inx
		dec	$fef4
		sec
  }
}
+		pla
++		rts

;	Send BYTE to drive:
.sendbyte_todrv

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		ldx	#$3f
		stx	$dd02			;Release bus
		bit	$dd00
		bpl	*-3
} else {
  !if (BF_DRIVE = 1541) {				;===== 1541
		ldx	#%11001000		;Release bus
		stx	$01
		bit	$01
		bpl	*-2
  } else {						;===== 1551
  }
}
		jmp	bitfire_send_byte_



;================================
;=	Drive side routines:
;================================



;=	Init / drive side:
.drv_init_start
!pseudopc $0500 {
.d_i_start

		jmp	.d_i_initstart
;	Set BUSY to computer:
.d_i_setbusy
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	#.BUSY
		sta	$1800
  } else {						;===== 1551
		lda	#.BUSY
		sta	$4000
		lda	#$ff
		sta	$4003			;set TCBM DATA port to output
  }
		rts

;	Set IDLE state to computer and return BF loop
.d_i_setidle
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	#.IDLE
		sta	$1800
  } else {						;===== 1551
		lda	#$00
		sta	$4003			;set TCBM DATA port to input
  }
		jmp	.pull_command

;	Send 2-bit status code to computer and return BF loop:
.d_i_sendstatus	txa
		and	#%00000011
		tax
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	.bin2ser ,x
		ora	#%00010000
		ldx	#.BLOCK_READY
		ldy	#.IDLE
		stx	$1800
		bit	$1800
		bpl	*-3
		sta	$1800
		bit	$1800
		bmi	*-3
		sty	$1800
  } else {						;===== 1551
		lda	#%00000001
		ldy	#.BLOCK_READY
		sty	$4000
		bit	$4002
		beq	*-3
		stx	$4000
		ldy	#$00
		bit	$4002
		bne	*-3
		sty	$4003			;set TCBM DATA port to input
  }
		jmp	.pull_command

;	BIN-to-GCR converter routine:
.d_i_bin2gcr	sta	.bin2gcr_cnt		;Save converting "block" no
.d_i_bin2gcr_cy	ldy	#0
-		lda	(.bin2gcr_rdp),y
		lsr
		lsr
		lsr
		lsr
		jsr	.d_i_bin2gcr_c
		lda	(.bin2gcr_rdp),y
		jsr	.d_i_bin2gcr_c
		iny
		cpy	#4
		bne	-
-		lda	.bin2gcr_buff,y
		sta	(.bin2gcr_wrp),y
		dey
		bpl	-
		clc
		lda	.bin2gcr_rdp + 0
		adc	#4
		sta	.bin2gcr_rdp + 0
		bcc	+
		inc	.bin2gcr_rdp + 1
+		clc
		lda	.bin2gcr_wrp + 0
		adc	#5
		sta	.bin2gcr_wrp + 0
		bcc	+
		inc	.bin2gcr_wrp + 1
+		dec	.bin2gcr_cnt
		bne	.d_i_bin2gcr_cy
		rts

.d_i_bin2gcr_c	and	#$0f
		tax
		lda	.dos_bin2gcr,x
		asl
		asl
		asl
		ldx	#4
-		asl
		rol	.bin2gcr_buff + 4
		rol	.bin2gcr_buff + 3
		rol	.bin2gcr_buff + 2
		rol	.bin2gcr_buff + 1
		rol	.bin2gcr_buff + 0
		dex
		bpl	-
		rts

;	Wait SYNC mark on disk:
.d_i_waitsync
		ldx	#0
  !if (BF_DRIVE = 1541) {				;===== 1541
		ldy	#15
-		bit	$1c00
		bpl	+			;If Sync present, ...
  } else {						;===== 1551
		ldy	#30
-		bit	$4002
		bvc	+
  }
		dex
		bne	-
		dey
		bne	-
		sec				;No sync, error
		rts
+		
  !if (BF_DRIVE = 1541) {				;===== 1541
		clv
  } else {						;===== 1551
		bit	$4001
  }
		ldy	#0
		clc				;Sync
		rts

;	Check block header on disk:
.d_i_waitblock	lda	#250
		sta	.sectorcnt
--		jsr	.d_i_waitsync
		bcs	.d_i_errornosy
-
  !if (BF_DRIVE = 1541) {				;===== 1541
		bvc	*
		clv
		lda	$1c01
  } else {						;===== 1551
		bit	$01
		bpl	*-2
		lda	$4001
  }
		cmp	.mem_headergcrb,y
		bne	+
		iny
		cpy	#8
		bne	-
		rts

+		dec	.sectorcnt
		bne	--
		ldx	#$02			;02: Sector not found
-		pla
		pla				;Drop return address
		jmp	.d_i_sendstatus
.d_i_errornosy	ldx	#$01			;01: No Sync
		bne	-			;BRA



.d_i_initstart					;### Init routine overwriteable at this position
		jsr	.d_i_setbusy
		lda	#$ff
		sta	.dirsect		;Directory sector destroyed in the memory, next load always re-read at the future
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda #.VIA2_LED_ON | .VIA2_MOTOR_ON
  } else {						;===== 1551
		lda	#.TCPU_LED_ON
  }
		jsr	.motor_on
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	$1c00
  } else {						;===== 1551
		lda	$01
  }
		and	#%00010000		;WPS?
		bne	+
		ldx	#$00			;00: WP On
		jmp	.d_i_sendstatus
+		jmp	.d_i_setidle

.d_i_end
.d_i_size = .d_i_end - .d_i_start;

                !byte >(.d_i_size - 1), <(.d_i_size - 1)
                !byte >(.d_i_end - 1),  <(.d_i_end - 1)
                !byte BITFIRE_UPLOAD
}
.drv_init_end
.drv_init_size = .drv_init_end - .drv_init_start



;=	Sector Write preparation / drive side:
.drv_writep_start
;!pseudopc $0600 {
!pseudopc .d_i_initstart {

.d_w_start
		jsr	.get_byte		;Get Track number
		sta	.to_track
		jsr	.get_byte		;Get Sector number
		sta	.mem_headerbinb + 2	;Sector Header: Sector No


.d_w_blgetcyc	jsr	.get_byte		;Get byte to sector data
.d_w_pointer	ldx	#$ff
		sta	.mem_blockbinb + 1 ,x	;Save to temporary block buffer
		beq	.d_w_blindrv
		dec	.d_w_pointer + 1
		jmp	.d_w_blgetcyc

.d_w_blindrv	jsr	.d_i_setbusy
		ldx	#$07
		stx	.mem_blockbinb + 0	;Sector Data: sign
		inx
		stx	.mem_headerbinb + 0	;Sector Header: sign
		lda	#$00
		sta	.mem_blockbinb + 258	;Sector Data: unused byte
		sta	.mem_blockbinb + 259	;Sector Data: unused byte
		tax
-		eor	.mem_blockbinb + 1 ,x
		inx
		bne	-
		sta	.mem_blockbinb + 257	;Sector Data: checksum

		ldx	.to_track
		stx	.mem_headerbinb + 3	;Sector Header: Track No
		lda	.head_lo + 2
		sta	.mem_headerbinb + 4	;Sector Header: ID1
		lda	.head_lo + 3
		sta	.mem_headerbinb + 5	;Sector Header: ID2
		eor	.head_lo + 2
		eor	.to_track
		eor	.mem_headerbinb + 2
		sta	.mem_headerbinb + 1	;Sector Header: checksum
		lda	#$0f
		sta	.mem_headerbinb + 6	;Sector Header: unused byte
		sta	.mem_headerbinb + 7	;Sector Header: unused byte

		cpx	#0			;Selected sector = 0?
		beq	+			;  If yes, no seek, ...
		cpx	#40+1			;Selected sector > 40?
		bcs	+			;  If yes, no seek, "sector not found" report lately
		jsr	.seek			;Seek to required track
+

		lda	#<.mem_headerbinb
		sta	.bin2gcr_rdp + 0
		lda	#>.mem_headerbinb
		sta	.bin2gcr_rdp + 1	;Header bin pointer
		lda	#<.mem_headergcrb
		sta	.bin2gcr_wrp + 0
		lda	#>.mem_headergcrb
		sta	.bin2gcr_wrp + 1	;Header GCR pointer
		lda	#2			;2×4 = 8 BYTEs to convert
		jsr	.d_i_bin2gcr

		lda	#<.mem_blockbinb
		sta	.bin2gcr_rdp + 0
		lda	#>.mem_blockbinb
		sta	.bin2gcr_rdp + 1	;Datas bin pointer
		lda	#<.mem_blockgcrb
		sta	.bin2gcr_wrp + 0
		lda	#>.mem_blockgcrb
		sta	.bin2gcr_wrp + 1	;Datas GCR pointer
		lda	#65			;65×4 = 260 BYTEs to convert
		jsr	.d_i_bin2gcr

		jmp	.d_i_setidle
.d_w_end
.d_w_size = .d_w_end - .d_w_start;

                !byte >(.d_w_size - 1), <(.d_w_size - 1)
                !byte >(.d_w_end - 1),  <(.d_w_end - 1)
                !byte BITFIRE_UPLOAD
}
.drv_writep_end
.drv_writep_size = .drv_writep_end - .drv_writep_start



;=	Sector Write "DoIt!" routine, write precalculated GCR datas to the selected block / drive side:
;=	The write algorithm is exactly same as the original DOS's routine,
;=	  including the head read/write switching method! :)
.drv_writed_start
;!pseudopc $0600 {
!pseudopc .d_i_initstart {
.d_d_start
		jsr	.d_i_setbusy
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	$1c00
  } else {						;===== 1551
		lda	$01
  }
		and	#%00010000		;WPS?
		bne	+
		ldx	#$00			;00: WP On
		jmp	.d_i_sendstatus

+		lda	#3
		sta	.writecnt		;Write Retry
.d_d_retrywrt	jsr	.d_i_waitblock		;Wait a block, if not found, not return here
		ldx	#9
-
  !if (BF_DRIVE = 1541) {				;===== 1541
		bvc	*
		clv
  } else {						;===== 1551
		bit	$01
		bpl	*-2
		bit	$4001
  }
		dex
		bne	-

  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	#$ff
		sta	$1c03			;Data / head switch to output
		lda	$1c0c
		and	#%00011111
		ora	#%11000000		;Mode line to Write
		sta	$1c0c
  } else {						;===== 1551
		lda	#$ff
		sta	$4004			;Data / head switch to output
		lda	$4002
		and	#%11101111		;Mode line to Write
		sta	$4002
  }
		lda	#$ff			;SYNC
		ldx	#5			;5×8 "1" bit = 40, sync
  !if (BF_DRIVE = 1541) {				;===== 1541
		sta	$1c01
		clv
-		bvc	*
		clv

  } else {						;===== 1551
		sta	$4001
		bit	$4001
-		bit	$01
		bpl	*-2
		bit	$4001
  }
		dex
		bne	-

		ldy	#<.mem_blockgcrb	;$06BB..$06FF / $0700..$07FF
-		lda	.mem_blockgcrb & 0xff00 ,y
  !if (BF_DRIVE = 1541) {				;===== 1541
		bvc	*
		clv
		sta	$1c01
  } else {						;===== 1551
		bit	$01
		bpl	*-2
		bit	$4001
		sta	$4001
  }
		iny
		bne	-

-		lda	$0100 + .mem_blockgcrb & 0xff00 ,y
  !if (BF_DRIVE = 1541) {				;===== 1541
		bvc	*
		clv
		sta	$1c01
  } else {						;===== 1551
		bit	$01
		bpl	*-2
		bit	$4001
		sta	$4001
  }
		iny
		bne	-

  !if (BF_DRIVE = 1541) {				;===== 1541
		bvc	*
		lda	$1c0c
		ora	#%11100000		;Switch back Mode to Read
		sta	$1c0c
		lda	#$00
		sta	$1c03			;Data / head switch back to input
  } else {						;===== 1551
		bit	$01
		bpl	*-2
		lda	$4002
		ora	#%00010000		;Switch back Mode to Read
		sta	$4002
		lda	#$00
		sta	$4004			;Data / head switch back to input
  }

		lda	#3
		sta	.verifycnt
.d_d_retryvfy	jsr	.d_i_waitblock		;Verify: Wait a block, if not found, not return here
		jsr	.d_i_waitsync
		bcc	+
		ldx	#$01			;01: No Sync
		jmp	.d_i_sendstatus
+
		ldy	#<.mem_blockgcrb	;$06BB..$06FF / $0700..$07FF
-		lda	.mem_blockgcrb & 0xff00 ,y
  !if (BF_DRIVE = 1541) {				;===== 1541
		bvc	*
		clv
		cmp	$1c01
  } else {						;===== 1551
		bit	$01
		bpl	*-2
		cmp	$4001
  }
		bne	.d_d_errorvfye		;If not same, retry...?
		iny
		bne	-

-		lda	$0100 + .mem_blockgcrb & 0xff00 ,y
  !if (BF_DRIVE = 1541) {				;===== 1541
		bvc	*
		clv
		cmp	$1c01
  } else {						;===== 1551
		bit	$01
		bpl	*-2
		cmp	$4001
  }
		bne	.d_d_errorvfye		;If not same, retry...?
		iny
		bne	-

		jmp	.d_i_setidle		;Verify OK, idle

.d_d_errorvfye	dec	.verifycnt
		bne	.d_d_retryvfy		;Retry verify
		dec	.writecnt
		beq	+
		jmp	.d_d_retrywrt		;Retry write
+		ldx	#$03			;03: Write error
		jmp	.d_i_sendstatus

.d_d_end
.d_d_size = .d_d_end - .d_d_start;

                !byte >(.d_d_size - 1), <(.d_d_size - 1)
                !byte >(.d_d_end - 1),  <(.d_d_end - 1)
                !byte BITFIRE_UPLOAD
}
.drv_writed_end
.drv_writed_size = .drv_writed_end - .drv_writed_start



;=	Exit routine: restore DIR block & exit / drive side:
.drv_exit_start
;!pseudopc $0600 {
!pseudopc .d_i_initstart {

.d_x_start	jsr	.d_i_setbusy
		jmp	.drivecode_launch

.d_x_end
.d_x_size = .d_x_end - .d_x_start;

                !byte >(.d_x_size - 1), <(.d_x_size - 1)
                !byte >(.d_x_end - 1),  <(.d_x_end - 1)
                !byte BITFIRE_UPLOAD
}
.drv_exit_end
.drv_exit_size = .drv_exit_end - .drv_exit_start






}

.sectrouts_end

}
}	;end macro definition
