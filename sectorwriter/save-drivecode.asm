;================================
;=	Drive side routines:
;================================

	!src "../config/config.inc"

!if (BF_DRIVE = 1541) {					;===== 1541
  !if (BITFIRE_SAVE_LEGACYMODE != 0) {		;Legacy mode: own status-send protocol
status_wpon	=	$00		;Write Protect On
status_nosync	=	$01		;No sync
status_sctrnotf	=	$02		;Sector not found
status_writeerr	=	$03		;Write error
  } else {					;New mode: use BF's BYTE-send routine
st_rdymsk	=	%00000010	;DAT = HiZ, CLK = Lo
status_wpon	=	$00 | st_rdymsk	;Write Protect On
status_nosync	=	$04 | st_rdymsk	;No sync
status_sctrnotf	=	$08 | st_rdymsk	;Sector not found
status_writeerr	=	$0c | st_rdymsk	;Write error
  }
} else {						;===== 1551
status_wpon	=	$00		;Write Protect On
status_nosync	=	$01		;No sync
status_sctrnotf	=	$02		;Sector not found
status_writeerr	=	$03		;Write error
}

z_bin2gcr_rdp	=	drivecode_preamb_lo + 0	;Bin-to-GCR Read pointer
z_bin2gcr_wrp	=	drivecode_preamb_lo + 2	;Bin-to-GCR Write pointer
z_bin2gcr_cnt	=	drivecode_preamb_lo + 4	;Bin-to-GCR converter counter
z_bin2gcr_buff	=	drivecode_preamb_hi	;Bin-to-GCR converter temp buffer
z_writecnt	=	drivecode_preamb_hi + 0
z_verifycnt	=	drivecode_preamb_hi + 1
z_sectorcnt	=	drivecode_preamb_hi + 2

_mem_blockbinb	=	$06fc			;Temporary bin. block storage area
_mem_blockgcrb	=	$06bb			;Temporary GCR block storage area
_mem_headerbinb	=	$06b3			;Temporary bin. header storage area
_mem_headergcrb	=	$06b1			;Temporary GCR header storage area



	* = $0000		;PC = 0: important labels as offsets



;=	Init / drive side:
drv_init_start_offs
!pseudopc drivecode_directory {

_d_i_start
		jmp	_dve_i_initstart
;	Set BUSY to computer:
_dve_i_setbusy
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	#drivecode_BUSY
		sta	$1800
  } else {						;===== 1551
		lda	#drivecode_BUSY
		sta	$4000
		lda	#$ff
		sta	$4003			;set TCBM DATA port to output
  }
		rts

;	Set IDLE state to computer and return BF loop
_dve_i_setidle
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	#drivecode_IDLE
		sta	$1800
  } else {						;===== 1551
		lda	#$00
		sta	$4003			;set TCBM DATA port to input
  }
		jmp	drivecode_pullcomm

;	Send 2-bit status code to computer and return BF loop:
;	X <- 0/1/2/3: error code (2 bit)
_dve_i_sendstatus
  !if (BF_DRIVE = 1541) {				;===== 1541
    !if (BITFIRE_SAVE_LEGACYMODE != 0) {		;Legacy mode: own status-send protocol
		txa
		and	#%00000011
		tax					;Masking not required - 1541 code included for binary compatibility
		lda	drivecode_41_bin2ser ,x
		ora	#%00010000
		ldx	#drivecode_BLOCK_RDY
		ldy	#drivecode_IDLE
		stx	$1800
		bit	$1800
		bpl	*-3
		sta	$1800
		bit	$1800
		bmi	*-3
		sty	$1800			;31
    } else {						;New mode: use BF's BYTE-send routine
		lda	#%00001111
		sta	drivecode_preamb_hi+0			;%1111ssrr => rr: READY signal, ss: status code, %1111: IDLE
		lda	#1
		sta	drivecode_41_pre_cmp			;1 BYTE to send
		lda	drivecode_41_preover
		pha
		lda	#$60					;RTS op.code
		sta	drivecode_41_preover
		lda	drivecode_41_bin2ser,x			;Status + READY sign => VIA register value
		ldy	#$00
		jsr	drivecode_41_presend_s			;Send STATUS
		pla
		sta	drivecode_41_preover
		bit	$1800
		bmi	*-3			;32		;Wait for ATN high (host accept the last bitpair)
    }
  } else {						;===== 1551
		lda	#%00000001
		ldy	#drivecode_BLOCK_RDY
		sty	$4000
		bit	$4002
		beq	*-3
		stx	$4000
		ldy	#$00
		bit	$4002
		bne	*-3
		sty	$4003			;set TCBM DATA port to input
  }
		jmp	drivecode_pullcomm

;	BIN-to-GCR converter routine:
_dve_i_bin2gcr	sta	z_bin2gcr_cnt		;Save converting "block" no
.d_i_bin2gcr_cy	ldy	#0
-		lda	(z_bin2gcr_rdp),y
		lsr
		lsr
		lsr
		lsr
		jsr	.d_i_bin2gcr_c
		lda	(z_bin2gcr_rdp),y
		jsr	.d_i_bin2gcr_c
		iny
		cpy	#4
		bne	-
-		lda	z_bin2gcr_buff,y
		sta	(z_bin2gcr_wrp),y
		dey
		bpl	-
		clc
		lda	z_bin2gcr_rdp + 0
		adc	#4
		sta	z_bin2gcr_rdp + 0
		bcc	+
		inc	z_bin2gcr_rdp + 1
+		clc
		lda	z_bin2gcr_wrp + 0
		adc	#5
		sta	z_bin2gcr_wrp + 0
		bcc	+
		inc	z_bin2gcr_wrp + 1
+		dec	z_bin2gcr_cnt
		bne	.d_i_bin2gcr_cy
		rts

.d_i_bin2gcr_c	and	#$0f
		tax
		lda	drivecode_dos_bin2gcr,x
		asl
		asl
		asl
		ldx	#4
-		asl
		rol	z_bin2gcr_buff + 4
		rol	z_bin2gcr_buff + 3
		rol	z_bin2gcr_buff + 2
		rol	z_bin2gcr_buff + 1
		rol	z_bin2gcr_buff + 0
		dex
		bpl	-
		rts

;	Wait SYNC mark on disk:
_dve_i_waitsync
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
		bit	$1c01			;Read PortA: prev. latched data accepted, ready for next latch
		clv
  } else {						;===== 1551
		bit	$4001
		bit	$4001
  }
		ldy	#0
		clc				;Sync
		rts

;	Check block header on disk:
_dve_i_waitblock
		lda	#250
		sta	z_sectorcnt
--		jsr	_dve_i_waitsync
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
		cmp	_mem_headergcrb,y
		bne	+
		iny
		cpy	#8
		bne	-
		rts

+		dec	z_sectorcnt
		bne	--
		ldx	#status_sctrnotf	;02: Sector not found
-		pla
		pla				;Drop return address
		jmp	_dve_i_sendstatus
.d_i_errornosy	ldx	#$01			;01: No Sync
		bne	-			;BRA



_dve_i_initstart
		jsr	_dve_i_setbusy
		lda	#$ff
		sta	drivecode_dirsect	;Directory sector destroyed in the memory, next load always re-read at the future
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	#drivecode_41_VIA2LED_ON | drivecode_41_VIA2MTR_ON
  } else {						;===== 1551
		lda	#drivecode_51_TCPULED_ON
  }
		jsr	drivecode_motoron
  !if (BITFIRE_SAVE_LEGACYMODE != 0) {		;Legacy mode: own status-send protocol
    !if (BF_DRIVE = 1541) {				;===== 1541
		lda	$1c00
    } else {						;===== 1551
		lda	$01
    }
		and	#%00010000		;WPS?
		bne	+
		ldx	#status_wpon		;00: WP On
		jmp	_dve_i_sendstatus
+
  }
		jmp	_dve_i_setidle
.d_i_end
.d_i_size = .d_i_end - _d_i_start;

                !byte >(.d_i_size - 1), <(.d_i_size - 1)
                !byte >(.d_i_end - 1),  <(.d_i_end - 1)
                !byte BITFIRE_UPLOAD
}
drv_init_end_offs
drv_init_size = drv_init_end_offs - drv_init_start_offs



;=	Sector Write preparation / drive side:
drv_writep_start_offs
!pseudopc _dve_i_initstart {

.d_w_start
		jsr	drivecode_getbyte	;Get Track number
		sta	drivecode_to_track
		jsr	drivecode_getbyte	;Get Sector number
		sta	_mem_headerbinb + 2	;Sector Header: Sector No


.d_w_blgetcyc	jsr	drivecode_getbyte	;Get byte to sector data
.d_w_pointer	ldx	#$ff
		sta	_mem_blockbinb + 1 ,x	;Save to temporary block buffer
		beq	.d_w_blindrv
		dec	.d_w_pointer + 1
		jmp	.d_w_blgetcyc

.d_w_blindrv	jsr	_dve_i_setbusy
		ldx	#$07
		stx	_mem_blockbinb + 0	;Sector Data: sign
		inx
		stx	_mem_headerbinb + 0	;Sector Header: sign
		lda	#$00
		sta	_mem_blockbinb + 258	;Sector Data: unused byte
		sta	_mem_blockbinb + 259	;Sector Data: unused byte
		tax
-		eor	_mem_blockbinb + 1 ,x
		inx
		bne	-
		sta	_mem_blockbinb + 257	;Sector Data: checksum

		ldx	drivecode_to_track
		stx	_mem_headerbinb + 3	;Sector Header: Track No
		lda	drivecode_head_lo + 2
		sta	_mem_headerbinb + 4	;Sector Header: ID1
		lda	drivecode_head_lo + 3
		sta	_mem_headerbinb + 5	;Sector Header: ID2
		eor	drivecode_head_lo + 2
		eor	drivecode_to_track
		eor	_mem_headerbinb + 2
		sta	_mem_headerbinb + 1	;Sector Header: checksum
		lda	#$0f
		sta	_mem_headerbinb + 6	;Sector Header: unused byte
		sta	_mem_headerbinb + 7	;Sector Header: unused byte

		cpx	#0			;Selected sector = 0?
		beq	+			;  If yes, no seek, ...
		cpx	#40+1			;Selected sector > 40?
		bcs	+			;  If yes, no seek, "sector not found" report lately
		jsr	drivecode_seek		;Seek to required track
+

		lda	#<_mem_headerbinb
		sta	z_bin2gcr_rdp + 0
		lda	#>_mem_headerbinb
		sta	z_bin2gcr_rdp + 1	;Header bin pointer
		lda	#<_mem_headergcrb
		sta	z_bin2gcr_wrp + 0
		lda	#>_mem_headergcrb
		sta	z_bin2gcr_wrp + 1	;Header GCR pointer
		lda	#2			;2×4 = 8 BYTEs to convert
		jsr	_dve_i_bin2gcr

		lda	#<_mem_blockbinb
		sta	z_bin2gcr_rdp + 0
		lda	#>_mem_blockbinb
		sta	z_bin2gcr_rdp + 1	;Datas bin pointer
		lda	#<_mem_blockgcrb
		sta	z_bin2gcr_wrp + 0
		lda	#>_mem_blockgcrb
		sta	z_bin2gcr_wrp + 1	;Datas GCR pointer
		lda	#65			;65×4 = 260 BYTEs to convert
		jsr	_dve_i_bin2gcr

		jmp	_dve_i_setidle
.d_w_end
.d_w_size = .d_w_end - .d_w_start;

                !byte >(.d_w_size - 1), <(.d_w_size - 1)
                !byte >(.d_w_end - 1),  <(.d_w_end - 1)
                !byte BITFIRE_UPLOAD
}
drv_writep_end_offs
drv_writep_size = drv_writep_end_offs - drv_writep_start_offs



;=	Sector Write "DoIt!" routine, write precalculated GCR datas to the selected block / drive side:
;=	The write algorithm is exactly same as the original DOS's routine,
;=	  including the head read/write switching method! :)
drv_writed_start_offs
!pseudopc _dve_i_initstart {
.d_d_start
		jsr	_dve_i_setbusy
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	$1c00
  } else {						;===== 1551
		lda	$01
  }
		and	#%00010000		;WPS?
		bne	+
		ldx	#status_wpon		;00: WP On
		jmp	_dve_i_sendstatus

+		lda	#3
		sta	z_writecnt		;Write Retry
.d_d_retrywrt	jsr	_dve_i_waitblock	;Wait a block, if not found, not return here
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

		ldy	#<_mem_blockgcrb	;$06BB..$06FF / $0700..$07FF
-		lda	_mem_blockgcrb & 0xff00 ,y
  !if (BF_DRIVE = 1541) {				;===== 1541
		bvc	*
		clv
		sta	$1c01
  } else {						;===== 1551
		bit	$01
		bpl	*-2
		;bit	$4001
		sta	$4001
  }
		iny
		bne	-

-		lda	$0100 + _mem_blockgcrb & 0xff00 ,y
  !if (BF_DRIVE = 1541) {				;===== 1541
		bvc	*
		clv
		sta	$1c01
  } else {						;===== 1551
		bit	$01
		bpl	*-2
		;bit	$4001
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
		sta	z_verifycnt
.d_d_retryvfy	jsr	_dve_i_waitblock	;Verify: Wait a block, if not found, not return here
		jsr	_dve_i_waitsync
		bcc	+
		ldx	#status_nosync		;01: No Sync
		jmp	_dve_i_sendstatus
+
		ldy	#<_mem_blockgcrb	;$06BB..$06FF / $0700..$07FF
-		lda	_mem_blockgcrb & 0xff00 ,y
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

-		lda	$0100 + _mem_blockgcrb & 0xff00 ,y
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
		cpy	#$fe			;All bytes checked?
		bne	-

		jmp	_dve_i_setidle		;Verify OK, idle

.d_d_errorvfye	dec	z_verifycnt
		bne	.d_d_retryvfy		;Retry verify
		dec	z_writecnt
		beq	+
		jmp	.d_d_retrywrt		;Retry write
+		ldx	#status_writeerr	;03: Write error
		jmp	_dve_i_sendstatus

.d_d_end
.d_d_size = .d_d_end - .d_d_start;

                !byte >(.d_d_size - 1), <(.d_d_size - 1)
                !byte >(.d_d_end - 1),  <(.d_d_end - 1)
                !byte BITFIRE_UPLOAD
}
drv_writed_end_offs
drv_writed_size = drv_writed_end_offs - drv_writed_start_offs



;=	Exit routine: (restore DIR block &) stop motor & exit / drive side:
drv_exit_start_offs
!pseudopc _dve_i_initstart {

.d_x_start	jsr	_dve_i_setbusy
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		jmp	drivecode_stopmotor
} else {
		jmp	drivecode_launch
}

.d_x_end
.d_x_size = .d_x_end - .d_x_start;

                !byte >(.d_x_size - 1), <(.d_x_size - 1)
                !byte >(.d_x_end - 1),  <(.d_x_end - 1)
                !byte BITFIRE_UPLOAD
}
drv_exit_end_offs
drv_exit_size = drv_exit_end_offs - drv_exit_start_offs
