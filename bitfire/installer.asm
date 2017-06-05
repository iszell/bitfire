!src "config.inc"
!convtab pet
!cpu 6510

!if ((BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC) or (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541DC) or (BITFIRE_PLATFORM = BITFIRE_C64)) {
	PLUS4_DRIVE = 1541
} else {
	PLUS4_DRIVE = 1551
}

!zone installer {
.dc_src		= $fc
.dc_dst		= $fe

.listen		= $ffb1
.listen_sa	= $ff93
.iecout		= $ffa8
.unlisten	= $ffae

		* = BITFIRE_INSTALLER_ADDR
		!src "loader_acme.inc"
.init_inst

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda #$37
		sta $01
		lda #$00
		sta $d015
} else {
		sta $ff3e
		lda #$00
}
		sta .iec_units

		lda #8
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		sta $ba
} else {
		sta $ae
}
		;jmp do_install

		ldx #4
-
		jsr .open_w_15
		bmi +
		inc .iec_units
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda $ba
} else {
		lda $ae
}
		sta .my_drive
		jsr .unlisten
+
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		inc $ba
} else {
		inc $ae
}
		dex
		bne -
.iec_units = * + 1
		lda #$00
		cmp #1
		beq .do_install
		ldx #$00
-
		lda .pebcak,x
		beq .init_inst
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		sta $07c0,x
} else {
		sta $0fc0,x
}
		inx
		bne -
.do_install
.my_drive = * + 1
		lda #$08

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		sta $ba
} else {
		sta $ae
}
		;install bootloader with fast m-w and onetime loader-init
		jsr .install_bootstrap

		sei

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda #$c3
		sta $dd00

		lda #$3f
		sta $dd02
} else {
  !if (PLUS4_DRIVE = 1541) {				;===== 1541
		lda #%11001000				  ;ATN/CLK/DATA drive Off (Cas. MTR Off)
		sta $01

		lda #%00001111				  ;ATN/CLK/DATA drive OUTPUT, CLK / DATA in INPUT (Cas. MTR OUTPUT, Cas. RD INPUT)
		sta $00
  } else {						;===== 1551
		ldx	#$30
		lda	$ae
		cmp	#8
		beq	+
		ldx	#$00
		cmp	#9
		beq	+
-		inc	$ff19
		jmp	-
+		stx	.install_51un+1
		lda	#%01000000
		sta	$fec5,x			;TCBM DAV out, ACK in
		sta	$fec2,x			;TCBM DAV 1
		lda	#%00000000
		sta	$fec4,x			;TCBM ST1/0 in
		sta	$fec0,x			;TCBM DATA = $00
		lda	#%11111111
		sta	$fec3,x			;TCBM DATA DDR: Datas to 1551
		lda	$fec2,x			;TCBM handshake lines
		bmi	*-3			;Wait ACK line 0 => 1551 ready to accept datas
  }
}

.cnt = * + 1
		lda #(>.drivecode_size) + 1
!if (PLUS4_DRIVE = 1541) {                  ;===== 1541
  !if (BITFIRE_PLATFORM = BITFIRE_C64) {
		bit $dd00		;wait until drive bootloader is active
		bmi *-3

		lda #$37
		sta $dd02
  } else {
		bit $01				;wait until drive bootloader is active
		bmi *-2

		lda #%11001100				  ;ATN drive On
		sta $01
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
 }

-
.dc_data = * + 1
		lda .drivecode_start
		sec
		ror
		sta .dc_src

  !if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda #$2f
.s_loop
		and #$2f						;clear bit 4 and 0..2 and waste some cycles here
		adc #$00						;on carry set, clear bit 4, else keep
		eor #$30
		ora #$0f

		sta $dd02
		pha				;make NTSC machines happy
		pla
		lsr .dc_src
		bne .s_loop
  } else {
		lda #%11001001				  ;ATN drive Off, DATA drive On
.s_loop
		and #%11001001				  ;CLK drive Off
		bcc +
		ora #%00000010				  ;If sended bit = 1: CLK drive On
+
		sta $01
		eor #%00000001				  ;DATA drive flip
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		lsr .dc_src
		bne .s_loop
  }
} else {						;=====	1551
-
.dc_data = * + 1
		lda	.drivecode_start+2	;WARNING: +2: Skip the CPU port DDR / PORT registers on the 1551 side
		sta	$fec0,x			;Data to drive
		lda	#%00000000		;ACK=0: Data valid
		sta	$fec2,x
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		lda	#$00
		sta	$fec0,x			;DATA = $00: no last BYTE
		lda	#%01000000		;ACK=1: One BYTE transfered
		sta	$fec2,x
		nop
		nop
		nop
		nop
}
		inc .dc_data
		bne +
		inc .dc_data+1
+
		lda .dc_data
		cmp #<.drivecode_end
		bne -
		lda .dc_data+1
		cmp #>.drivecode_end
		bne -

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda #$37			;raise atn to signal end of transfer
		sta $dd02
} else {
  !if (PLUS4_DRIVE = 1541) {				;===== 1541
		lda #%11001100				  ;ATN drive On, CLK/DATA drive Off
		sta $01
  } else {						;===== 1551
		lda	#$ff			;Last BYTE transfered, start code in 1551!
		sta	$fec0,x			;Data to drive
		lda	#%00000000		;ACK=0: Data valid
		sta	$fec2,x
		nop
		nop
		nop
		nop
		lda	#$ff
		sta	$fec0,x			;DATA = $FF: This is last BYTE
		lda	#%01000000		;ACK=1: One (and last) BYTE transfered
		sta	$fec2,x
  }
}

!if (BITFIRE_RESIDENT_AUTOINST != 0) {
!if (bitfire_resident_size) < 256 {
		;better force to 8 bit, label might be defined as 16 bit
		ldx #<(bitfire_resident_size)
-
		lda .res_start,x
		sta BITFIRE_RESIDENT_ADDR,x
		dex
	!if bitfire_resident_size >= $80 {
		cpx #$ff
		bne -
	} else {
		bpl -
	}
} else {
		;copy resident part
		ldx #$00
-
		lda .res_start,x
		sta BITFIRE_RESIDENT_ADDR,x
	!if bitfire_resident_size > $200 {
		lda .res_start+$100,x
		sta BITFIRE_RESIDENT_ADDR+$100,x
	}
		lda .res_start + ((bitfire_resident_size) - $100),x
		sta BITFIRE_RESIDENT_ADDR + ((bitfire_resident_size) - $100),x
		dex
		bne -
}
}


!if (BITFIRE_PLATFORM = BITFIRE_C64) {
;.l1		lda $d012
;.l2		cmp $d012
;		beq .l2
;		bmi .l1
;		cmp #$20
;		bcs .nontsc

;		lda #$b9		;lda $xxxx,y
;		sta bitfire_ntsc_fix1
;		lda #$19		;ora $xxxx,y
;		sta bitfire_ntsc_fix2
;		sta bitfire_ntsc_fix3
;		lda #$39		;and $xxxx,y
;		sta bitfire_ntsc_fix4
;
;		lda #-$37
;		sta bitfire_ntsc_fix1 + 1
;		sta bitfire_ntsc_fix2 + 1
;		sta bitfire_ntsc_fix3 + 1
;		sta bitfire_ntsc_fix4 + 1
;
;		lda #$dc
;		sta bitfire_ntsc_fix1 + 2
;		sta bitfire_ntsc_fix2 + 2
;		sta bitfire_ntsc_fix3 + 2
;		sta bitfire_ntsc_fix4 + 2
;.nontsc

		lda #$3f			;drop atn to signal end of transfer
		sta $dd02
} else {
  !if (PLUS4_DRIVE = 1541) {				;===== 1541
		lda #%11001000						  ;ATN/CLK/DATA drive Off
		sta $01
  } else {						;===== 1551
  }
}

!if BITFIRE_AUTODETECT = 1 {
		!src "detect.asm"
}
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda #$7f
		sta $dd0d
		lda $dd0d
}

		;wait until floppy is ready
		;wait for drive to initialize XXX TODO maybe wait for special signal on $dd00?

;		sei
;		ldx #$10
;wait
;-
;		bit $d011
;		bpl *-3
;		bit $d011
;		bmi *-3
;		dex
;		bpl -

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
-
		lda $dd00
		bpl -
} else {
  !if (PLUS4_DRIVE = 1541) {				;===== 1541
-
		lda $01
		bpl -
  } else {						;===== 1551
.install_51un	ldx	#$00			;Self-modified: $30 (U8) / $00 (U9)
		lda	$fec2,x
		bmi	*-3
  }
}
		rts

.open_w_15
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda $ba
} else {
		lda $ae
}
		jsr .listen
		lda #$00
		sta $90
		lda #$6f
		jmp .listen_sa

.install_bootstrap
		jsr .open_w_15
		lda #'i'
		jsr .iecout
		jsr .unlisten
		;ldx #$10
		;jsr wait
		;install first routines via m-w
		lda #<.bootstrap_start
		sta .dc_src
		lda #>.bootstrap_start
		sta .dc_src+1

		lda #<.bootstrap
		sta .dc_dst
		lda #>.bootstrap
		sta .dc_dst+1

		ldx #(.bootstrap_size / $20) + 1
.bs_loop
		jsr .open_w_15

		lda #'m'
		jsr .iecout
		lda #'-'
		jsr .iecout
		lda #'w'
		jsr .iecout
		lda .dc_dst		;target-address
		jsr .iecout
		lda .dc_dst+1
		jsr .iecout
		lda #$20	;payload
		jsr .iecout

		ldy #$00
-
		lda (.dc_src),y
		jsr .iecout
		iny
		cpy #$20
		bne -

		tya
		clc
		adc .dc_dst
		sta .dc_dst
		bcc *+4
		inc .dc_dst+1

		tya
		clc
		adc .dc_src
		sta .dc_src
		bcc *+4
		inc .dc_src+1

		jsr .unlisten

		dex
		bne .bs_loop

		;now execute installer
		jsr .open_w_15

		;ldx #$00
-
		lda .me_code,x
		jsr .iecout
		inx
		cpx #$05
		bne -
		jmp .unlisten

.pebcak
!convtab scr {
		!text "more than 1 drive on bus, turn off plz!"
		!byte 0
}

.me_code
!byte $4d,$2d,$45,<.bootstrap_run,>.bootstrap_run

!src "drivecode.asm"

!if (BITFIRE_RESIDENT_AUTOINST != 0) {
.res_start
!bin "resident",,2
}
}
