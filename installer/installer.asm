!convtab pet
!cpu 6510

z_instlr	=	$02		;=== 10 BYTEs ZP for installer usage
z_ptr		=	z_instlr+0
z_ptrl		=	z_ptr+0
z_ptrh		=	z_ptr+1
z_src		=	z_instlr+2
z_srcl		=	z_src+0
z_srch		=	z_src+1
z_dest		=	z_instlr+4
z_destl		=	z_dest+0
z_desth		=	z_dest+1
z_tmp		=	z_instlr+6
z_tmpl		=	z_tmp+0
z_tmph		=	z_tmp+1
z_byte		=	z_instlr+8

!if (BITFIRE_DRIVESELECTOR = 1) {
z_unsel		=	$10		;=== More BYTE-s ZP for Unit Selector usage
}






listen       = $ffb1
listen_sa    = $ff93
iecout       = $ffa8
unlisten     = $ffae
talk         = $ffb4
talk_sa      = $ff96
iecin        = $ffa5
untalk       = $ffab

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
z_c3po		=	$94		;Serial: UsedBuffer
z_r2d2		=	$a3		;Serial: SendEOI
z_sa		=	$b9		;Serial: Secondary Address / KERNAL
z_fa		=	$ba		;Serial: Device number / KERNAL
z_ndx		=	$c6		;B Keyboard buffer index
_keyd		=	$0277		;B×10 Interrupt's Keyboard buffer
_shflag		=	$028d		;"Shift" flags: B0=1: Shift, B1=1: C= pressed
} else {
z_c3po		=	$94		;Serial: UsedBuffer
z_r2d2		=	$a6		;Serial: SendEOI
z_sa		=	$ad		;Serial: Secondary Address / KERNAL
z_fa		=	$ae		;Serial: Device number / KERNAL
z_ndx		=	$ef		;B Keyboard buffer index
z_usekdy	=	$f9		;TCBM Listen/Talk flag
z_curbnk	=	$fb		;Rom bank number
_keyd		=	$0527		;B×10 Interrupt's Keyboard buffer
_shflag		=	$0543		;"Shift" flags: B0=1: Shift, B1=1: C= pressed
}

z_st		=	$90		;STatus



!if (BITFIRE_PLATFORM = BITFIRE_C64) {
	BITFIRE_INSTALLER_ADDR = BITFIRE_INSTALLER_C64_ADDR
} else {
	BITFIRE_INSTALLER_ADDR = BITFIRE_INSTALLER_PLUS4_ADDR
}

	* = BITFIRE_INSTALLER_ADDR


bitfire_install_

		jmp	__start__

!if (BF_DRV_SD2IEC = 1) {
bitfire_setswaplist
		jmp	inst_setswaplst
}

!if (BITFIRE_DRIVESELECTOR = 1) {
bitfire_setbranding
		jmp	unitsel_setbranding
}

__start__
		sei
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#$37
		sta	$01
		lda	#$00
		sta	$d015			;Disable all sprites
} else {
		sta	$ff3e
		lda	#0
		sta	z_curbnk		;Clear current ROM bank number
}

!if (BITFIRE_DRIVESELECTOR = 1) {
		jsr	unitsel_init		;Inicialize unit selector
}

		lda	z_fa			;Last used unit
		tax
		and	#%11111100
		cmp	#$08			;8/9/10/11?
		beq	.lastunit_ok
		ldx	#0			;No correct "Last Used Unit"
.lastunit_ok	stx	_loadfromunit		;Save "Last Used Unit" no / 0, if unknown

		lda	#0
		sta	_firstscan
!if (BITFIRE_DRIVESELECTOR = 1) {
		sta	_discoverw		;"Silent" scanning first
}
		jsr	rom_scnkey		;Scan keyboard
		lda	_shflag			;"Shift" flags
		and	#%00000010		;"C=" pressed?
		bne	.cbmpress		;If yes, "silent scan" skipped
		jsr	unit_scanner
.cbmpress

		lda	_all_units		;Founded units
		cmp	#1			;Only one?
		bne	.notonlyone		;If not, unit selector will be start
		lda	_lastunitno		;Supported unit no
		beq	.notonlyone		;If none, unit selector will be start
		sta	_selecteddrvno		;Set only one supported drive unit#
		lda	_lastunittype
		sta	_selecteddrvtyp		;Set drive type
		bne	.onlyonedrive		;~BRA
.notonlyone

!if (BITFIRE_DRIVESELECTOR = 1) {
		jsr	unit_selector		;Unit selector UI
} else {
  !serious "No unit selector!"
}

.onlyonedrive	jsr	do_installer
!if (BITFIRE_AUTODETECT = 1) {
		!src	"detect.asm"
}
		rts



;	Download and execute drivecode to selected drive
;	Copy resident to requested memory area
do_installer	lda	_selecteddrvno
		sta	z_fa
		lda	_selecteddrvtyp
		and	#%00001111
!if (BF_DRV_1541 = 1) {
!src "../drive/bootstrap-1541.inc"
		cmp	#bus_1541
		beq	.install1541
		cmp	#bus_1571
		beq	.install1541
		jmp	.checknext1
.install1541	ldx	#<.bs1541
		ldy	#>.bs1541
		jsr	install_bootstrap_cbm
		jsr	setwaitbootstrap
		ldx	#<.drivecode41_e
		ldy	#>.drivecode41_e
		stx	z_destl
		sty	z_desth
		ldx	#<.drivecode41
		ldy	#>.drivecode41
		jsr	senddrivecode
		jsr	screen_restore		;Restore screen and disable interrupt
		ldx	#<resi1541
		ldy	#>resi1541
  !if (BITFIRE_PLATFORM = BITFIRE_C64) {
		jmp	copyresident		;Copy resident
  } else {
    !if (MULTI_SWAP_INST = 2) {
		jsr	copyresident		;Copy resident
		ldx	#<.resi1541scswp
		ldy	#>.resi1541scswp
		jsr	copyresident		;Copy swapper
		jmp	swapper_plus4_swap_receiver	;Swap receiver: set "Single clock mode" default
    } else {
		jmp	copyresident		;Copy resident
    }

    !if (MULTI_SWAP_INST = 2) {
		!src	"../resident/swaprecv-c264-41db.inc"
.resi1541scswp	!word	bitfire_swaprecv_addr
		!word	.resi1541scswp_e
		!bin	"../resident/swaprecv-c264-41db.bin"
.resi1541scswp_e
bitfire_plus4_swap_receiver = swapper_plus4_swap_receiver
    }
  }

.bs1541		!word	.bs1541bin
		!word	bootstrap_41_start
		!byte	(bootstrap_41_size / $20) + 1
		!word	bootstrap_41_run
.bs1541bin	!bin	"../drive/bootstrap-1541.bin"
.drivecode41	!bin	"../drive/drivecode-1541.bin"
.drivecode41_e
}
.checknext1



!if (BF_DRV_1551 = 1) {
!src "../drive/bootstrap-1551.inc"
		cmp	#bus_1551
		beq	.install1551
		jmp	.checknext2
.install1551	ldx	#<.bs1551
		ldy	#>.bs1551
		jsr	install_bootstrap_cbm

		ldx	#$30
		lda	z_fa
		cmp	#8
		beq	+
		ldx	#$00
		cmp	#9
		beq	+
		jmp	($fffc)
+		lda	#%01000000
		sta	$fec5,x			;TCBM DAV out, ACK in
		sta	$fec2,x			;TCBM DAV 1
		lda	#%00000000
		sta	$fec4,x			;TCBM ST1/0 in
		sta	$fec1,x			;TCBM ST1/0 = 0, if switch DIR to output
		sta	$fec0,x			;TCBM DATA = $00
		lda	#%11111111
		sta	$fec3,x			;TCBM DATA DDR: Datas to 1551
		lda	$fec2,x			;TCBM handshake lines
		bmi	*-3			;Wait ACK line 0 => 1551 ready to accept datas
		lda	#%00000001
		sta	$fec4,x			;ST0 = Out, 0

		lda	#<.drivecode51_e
		sta	z_destl
		lda	#>.drivecode51_e
		sta	z_desth
		lda	#<(.drivecode51+2)
		sta	z_srcl
		lda	#>(.drivecode51+2)	;Skip CPU port
		sta	z_srch

		ldy	#0
-		lda	(z_src),y
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
		jsr	nextaddrs
		bne	-

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

		lda	$fec2,x			;TCBM handshake lines
		bpl	*-3			;Wait ACK line 1 => 1551 ready to accept datas
		jsr	screen_restore		;Restore screen and disable interrupt

		ldx	#<.resi1551
		ldy	#>.resi1551
		jsr	copyresident		;Copy resident
  !if (MULTI_SWAP_INST = 2) {
		jsr	copyresident_cn		;Copy continue (swapper)
  }
  !if (BITFIRE_1551_ONLY8 != 1) {
		lda	z_fa
		cmp	#8
		beq	.no1551patch
		lda	#<BITFIRE_RESIDENT_ADDR
		sta	z_srcl
		lda	#>BITFIRE_RESIDENT_ADDR
		sta	z_srch
		ldy	#1
.patch51cyc	lda	(z_src),y
		cmp	#$fe			;TCBM port address Hi BYTE?
		bne	.patch51_no
		dey
		lda	(z_src),y
		and	#%11111000		;$FEF0..$FEF7 -> $E0..$E7 -> %11100xxx
		cmp	#$f0
		bne	.patch51_nothis
		lda	(z_src),y
		and	#%11001111		;$FEFx -> $FECx
		sta	(z_src),y
.patch51_nothis	iny
.patch51_no	iny
		bne	.patch51cyc
.no1551patch
  }
		rts

.bs1551		!word	.bs1551bin
		!word	bootstrap_51_start
		!byte	(bootstrap_51_size / $20) + 1
		!word	bootstrap_51_run
.bs1551bin	!bin	"../drive/bootstrap-1551.bin"
.drivecode51	!bin	"../drive/drivecode-1551.bin"
.drivecode51_e

.resi1551	!word	BITFIRE_RESIDENT_ADDR
		!word	.resi1551_e
  !if (BF_PLUS4_BINCOMP = 1) {
		!src	"../resident/resident-c264-51db.incc"
		!bin	"../resident/resident-c264-51db.bin"
  } else {
		!src	"../resident/resident-c264-51dc.inc"
		!bin	"../resident/resident-c264-51dc.bin"
  }
.resi1551_e
  !if (MULTI_SWAP_INST = 2) {
		!word	bitfire_swaprecv_addr
		!word	.resi1551swp_e
		rts				;Swap routine = one RTS (for compatibility).resi1551swp_e
.resi1551swp_e
  }
}
.checknext2



!if (BF_DRV_SD2IEC = 1) {
!src "../drive/bootstrap-fsdv.inc"
		cmp	#bus_sd2iec
		beq	.installsd2iec
		jmp	.checknext3
.installsd2iec	bit	.bssd2iecbin		;Swap-list?
		bmi	.instsd2iec_nsl		;If no swaplist, setting skipped
		lda	#$6f			;Drive command channel
		jsr	opentolisten
		ldx	#0
.instsd2iec_slc	lda	.swaplisttxt,x
		beq	.instsd2iec_sle
		jsr	iecout
		inx
		bne	.instsd2iec_slc		;~BRA
.instsd2iec_sle	jsr	unlisten		;"XS:...": Set image (/directory) swaplist
		jsr	readanswerfromdrive	;Read back status
		lda	_statusstring+0
		cmp	#'0'
		bne	.instsd2iec_err
		lda	_statusstring+1
		cmp	#'0'
		bne	.instsd2iec_err		;"00, OK,00,00"? If not, inicializing error
.instsd2iec_nsl	ldx	#<.bssd2iec
		ldy	#>.bssd2iec
		jsr	install_bootstrap_vcpu
		jsr	setwaitbootstrap
		ldx	#<.drivecodesd2i_e
		ldy	#>.drivecodesd2i_e
		stx	z_destl
		sty	z_desth
		ldx	#<.drivecodesd2i
		ldy	#>.drivecodesd2i
		jsr	senddrivecode
		jsr	screen_restore		;Restore screen and disable interrupt
		lda	#BITFIRE_DRIVE_SD2IEC
		ora	link_chip_types
		sta	link_chip_types		;SD2IEC drive noted
		ldx	#<resi1541
		ldy	#>resi1541
  !if (BITFIRE_PLATFORM = BITFIRE_C64) {
    !if (BITFIRE_FSDV_BITS = 2) {
		jmp	copyresident		;Copy resident
    } else {
!src "../resident/resident-c64-41.incp"
		jsr	copyresident		;Copy resident
		ldx	#<.onebitpatch
		ldy	#>.onebitpatch
		jmp	copyresident		;Patch resident code
    }
  } else {
    !if (BITFIRE_FSDV_BITS = 1) {
		jsr	copyresident		;Copy resident
!src "../resident/resident-c264-41db.incp"
		ldx	#<.onebitpatch
		ldy	#>.onebitpatch
		jsr	copyresident		;Patch resident code
		jmp	copyresident_cn		;Copy swapper (one RTS)
    } else {
      !if (MULTI_SWAP_INST = 2) {
		jsr	copyresident		;Copy resident
		ldx	#<.resi1541scswp
		ldy	#>.resi1541scswp
		jsr	copyresident		;Copy swapper
		jmp	swapper_plus4_swap_receiver	;Swap receiver: set "Single clock mode" default
      } else {
		jmp	copyresident		;Copy resident
      }
    }
  }

.instsd2iec_err	jmp	($fffc)

.swaplisttxt	!text	"xs:autoswap.lst",0,0,0,0,0,0		;Some space to longer names...

.bssd2iec	!word	.bssd2iecbin
		!word	bootstrap_fs_start
		!byte	bootstrap_fs_size
		!word	bootstrap_fs_run
.bssd2iecbin	!bin	"../drive/bootstrap-fsdv.bin"
.drivecodesd2i	!bin	"../drive/drivecode-fsdv.bin"
.drivecodesd2i_e

  !if (BITFIRE_FSDV_BITS = 1) {
.onebitpatch	!word	fsdvp_getonebyte_addr
		!word	.onebitpatch_e
    !if (BITFIRE_PLATFORM = BITFIRE_C64) {
		!bin	"../resident/fsdv-1bitpatch-c64.bin"
    } else {
		!bin	"../resident/fsdv-1bitpatch-c264.bin"
    }
.onebitpatch_e
    !if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		!word	bitfire_swaprecv_addr
		!word	.onebitpatchswp_e
		rts
.onebitpatchswp_e
    }
  }

;	Set swaplist parameters:
;	Y:X <- swaplist file name (max. length: 16 characters with extension, zero terminated string)
;	A   <- default image number (B7=1: NO swaplist)
inst_setswaplst	sta	.bssd2iecbin		;Set image number
		ora	#$00
		bmi	.swaplistst_ok		;If no swaplist file, no name copy
  !if (BITFIRE_FSDV_IMAGEOFFSET != 0) {
		sec
		sbc	#(BITFIRE_FSDV_IMAGEOFFSET & $ff)
		sta	.bssd2iecbin		;Set modified image number
  }
		stx	.swaplistst+1
		sty	.swaplistst+2
		txa
		ora	.swaplistst+2		;Address valid?
		beq	.swaplistst_ok		;If not valid, not modified default name

		ldx	#0
.swaplistst	lda	$ffff,x
		sta	.swaplisttxt+3,x
		beq	.swaplistst_ok
		inx
		cpx	#16
		bne	.swaplistst
.swaplistst_ok	rts
}



.checknext3	jmp	($fffc)			;Comes here for incorrect implementation



;	1541 resident part, required for 1541 and SD2IEC:
!if (BF_DRV_1541 + BF_DRV_SD2IEC != 0) {
resi1541	!word	BITFIRE_RESIDENT_ADDR
		!word	.resi1541_e
  !if (BITFIRE_PLATFORM = BITFIRE_C64) {
		!src	"../resident/resident-c64-41.inc"
		!bin	"../resident/resident-c64-41.bin"
  } else {
    !if (BF_PLUS4_BINCOMP = 1) {
		!src	"../resident/resident-c264-41db.incc"
		!bin	"../resident/resident-c264-41db.bin"
    } else {
      !if (MULTI_SWAP_INST = 1) {
		!src	"../resident/resident-c264-41sc.inc"
		!bin	"../resident/resident-c264-41sc.bin"
      } else {
		!src	"../resident/resident-c264-41dc.inc"
		!bin	"../resident/resident-c264-41dc.bin"
      }
    }
  }
.resi1541_e
;  !if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
;resi1541swp	!word	bitfire_swaprecv_addr
;		!word	.resi1541swp_e
;		rts				;Swap routine = one RTS (for compatibility)
;.resi1541swp_e
;  }
}



!if (BF_DRV_1541 + BF_DRV_SD2IEC != 0) {

;	Set ports and wait bootstrap for ready:
setwaitbootstrap
  !if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#%11000011		;$c3
		sta	$dd00
		;lda	#%00101111		;$2F: CLK Lo
		lda	#%00011111		;$1F: DAT Lo
		sta	$dd02
		bit	$dd00
		;bmi	*-3			;Wait for DAT low
		bvs	*-3			;Wait for CLK low
		lda	#%00111111		;$3F: ATN/CLK/DAT HiZ
		sta	$dd02
  } else {
		;lda	#%11001010		;ATN/DAT drive Off, CLK drive On (Cas. MTR Off)
		lda	#%11001001		;ATN/CLK drive Off, DAT drive On (Cas. MTR Off)
		sta	$01
		lda	#%00001111		;ATN/CLK/DATA drive OUTPUT, CLK / DATA in INPUT (Cas. MTR OUTPUT, Cas. RD INPUT)
		sta	$00
		bit	$01
		;bmi	*-2			;Wait for DAT low
		bvs	*-2			;Wait for CLK low
		lda	#%11001000		;ATN/CLK/DAT HiZ
		sta	$01
  }
		rts

;	Send BYTE to device in serial bus:
sendbytetobootstrap
		sec
		ror
		sta	z_byte
  !if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#%00101111		;$2f (CLK DDR bit 0, CLK line 0)
.s_loop		and	#%00101111		;$2f clear bit 4 and 0..2 and waste some cycles here
		adc	#$00			;on carry set, clear bit 4, else keep (%00101111 / %00110000)
		eor	#%00110000		;$30 (%00n11111 / %00n00000)
		ora	#%00001111		;$0f (%00n11111 / %00n01111, B4 = sended bit, B5 = negated prev. value)
		sta	$dd02			;Send bit in CLK, DAT toggles
		pha				;make NTSC machines happy
		pla
		lsr	z_byte
		bne	.s_loop
  } else {
		lda	#%11001001		;ATN drive Off, DATA drive On
.s_loop		and	#%11001001		;CLK drive Off
		bcc	+
		ora	#%00000010		;If sended bit = 1: CLK drive On
+		sta	$01			;Send bit in CLK, DAT toggles
		eor	#%00000001		;DATA drive flip
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
		lsr	z_byte
		bne	.s_loop
  }
		rts

;	Send drivecode to bootstrap:
;	Y:X <- Start address in compy ram
;	z_dest <- end address in compy ram
senddrivecode	stx	z_srcl
		sty	z_srch
		ldy	#0
-		lda	(z_src),y
		jsr	sendbytetobootstrap
		jsr	nextaddrs
		bne	-
		rts

}

;	Next addr:
nextaddrs	inc	z_srcl
		bne	*+4
		inc	z_srch
		lda	z_srcl
		cmp	z_destl
		bne	+
		lda	z_srch
		cmp	z_desth
+		rts


;	Restore screen and disable interrupt:
screen_restore
!if (BITFIRE_DRIVESELECTOR = 1) {
		cli
		jsr	unitsel_restorescreen	;Restore screen
}
		sei
		rts

;	Copy resident part to the requested area:
;	Y:X <- Datas addr
copyresident	stx	z_srcl
		sty	z_srch
copyresident_cn	ldy	#0
		jsr	.readbyte
		sta	z_destl
		jsr	.readbyte
		sta	z_desth			;Destination address set
		jsr	.readbyte
		sta	z_ptrl
		jsr	.readbyte
		sta	z_ptrh			;End address set, source address setted
.copycyc	jsr	.readbyte
		sta	(z_dest),y
		inc	z_destl
		bne	*+4
		inc	z_desth
		lda	z_srcl
		cmp	z_ptrl
		bne	.copycyc
		lda	z_srch
		cmp	z_ptrh
		bne	.copycyc
		rts

.readbyte	lda	(z_src),y
		inc	z_srcl
		bne	*+4
		inc	z_srch
		rts

!src "bootstrapcopy.asm"
!src "scanner.asm"

!if (BITFIRE_DRIVESELECTOR = 1) {
  !src "cds/unitselector.asm"
}
