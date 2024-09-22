;	Device scanner:

;	Device types:
;bus_none	= 0
bus_prn		= 1
bus_unknown	= 2
bus_1541	= 3
bus_1551	= 4
bus_1571	= 5
bus_1581	= 6
bus_sd2iec	= 7



unit_scanner

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	#%00000111
		sta	$dd00
		lda	#%00111111
		sta	$dd02
} else {
		lda	#%00001000
		sta	$01
		lda	#%00001111
		sta	$00
}
		lda	#0
		sta	_ser_units		;Clear no. of SERIAL units
		sta	_prn_units		;Clear no. of SERIAL printer/plotter/unknown units
		sta	_all_units		;Clear no. of ALL units
		sta	_lastunitno		;Clear last checked and supported unit No
		sta	_lastunittype		;Clear last checked and supported unit type
		sta	_scanbreak		;Clear scan break flag
!if BITFIRE_UNITSCAN_PRN = 1 {
		ldx	#12-1			;12 device
} else {  
		ldx	#8-1			;8 device
}
.devqueryclr	sta	_devices,x
		dex
		bpl	.devqueryclr		;Clear device table

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda	$dd00			;Read serial bus lines
} else {
		lda	$01			;Read serial bus lines
}
		and	#%11000000		;DAT/CLK
		cmp	#%11000000		;DAT/CLK line high?
		beq	.scanstart
		dec	_scanbreak		;Set scan break flag
		bne	.scanrunonce		;~BRA

.scanstart
!if BITFIRE_UNITSCAN_PRN = 1 {
		lda	#4			;Start query unit no 4
} else {  
		lda	#8			;Start query unit no 8
}
		sta	z_fa

;	Unit check cycle:
.unitcheckcycle
!if BITFIRE_DRIVESELECTOR = 1 {
		lda	_discoverw
		sta	_itcounter
.ucwait		lda	_itcounter
		bne	.ucwait
}
		jsr	.unitcheck
		inc	z_fa			;Next unit
		lda	z_fa
		cmp	#16			;All units (4;8..15) checked?
		bne	.unitcheckcycle
.scanrunonce	lda	#$ff
		sta	_firstscan		;<>0: Query complete (at least once)
		rts

;	One unit check:
.unitcheck	lda	#$00
		sta	z_st			;ST=0
		sta	z_c3po
		sta	z_r2d2
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		sta	z_usekdy
}

!if BITFIRE_UNITSCAN_PRN = 1 {
		lda	#%00001000
		bit	z_fa			;Unit no >= 8? (Drive check?)
		bne	.unitdrivechk
		lda	#$60+0			;"OPEN x,4(5/6/7),0": normal mode
		jsr	opentolisten
		jsr	.stcheck		;Check ST
		lda	#13+128			;"Shift+Return"
		jsr	iecout
		jsr	.stcheck		;Check ST
		jsr	unlisten
		jsr	.stcheck		;Check ST
		inc	_all_units		;+1 device found
		inc	_prn_units		;+1 Printer/Plotter device
		inc	_ser_units		;+1 device on serial bus
		lda	#%00000000+bus_prn	;1: Printer/plotter (maybe)
		bne	.setdevice		;BRA
.unitdrivechk
}
		lda	#$6f			;Drive command channel
		jsr	opentolisten
		jsr	.stcheck		;Check ST
		lda	#'u'			;Send "UI"
		jsr	iecout
		lda	#'i'
		jsr	iecout
		jsr	.stcheck		;Check ST
		jsr	unlisten		;"UI": Soft reset
		jsr	.stcheck		;Check ST
		jsr	readanswerfromdrive	;Read back status
		jsr	.stcheck		;Check ST
		jsr	checkdevicetype		;Check device type
!if (BF_DRV_SD2IEC = 1) {
		cmp	#bus_sd2iec
		bne	.setdevice
		jsr	checkvcpusupport	;SD2IEC drive, check VCPU support
}
.setdevice	ldx	z_fa
!if BITFIRE_UNITSCAN_PRN = 1 {
		sta	_devices-4,x		;Save (/ clear) device type
} else {
		sta	_devices-8,x		;Save (/ clear) device type
}
		rts

;	Check ST:
.stcheck	lda	z_st
		and	#%10000011
		bne	.stcheck_nd
		rts
.stcheck_nd	pla
		pla
		rts



;	Read answer (status, ...) from drive:
readanswerfromdrive
		ldx	#statlen-1
		lda	#0
.statusclr	sta	_statusstring,x
		dex
		bpl	.statusclr

		lda	#%00000000
		sta	z_st
		lda	z_fa			;Unit No
		jsr	talk
		lda	#$6f			;Error channel
		sta	z_sa
		jsr	talk_sa
		ldy	#0
.readdata	bit	z_st
		bvs	.dataend
		jsr	iecin
		sta	_statusstring,y
		iny
		cpy	#statlen
		bne	.readdata
.dataend	jmp	untalk



;	Check device type:
checkdevicetype	inc	_all_units		;+1 device found
		inc	_ser_units		;+1 device on serial bus (if not serial, decrement later)
		lda	#<devicestrings
		sta	z_tmpl
		lda	#>devicestrings
		sta	z_tmph
		ldx	#0
		stx	.typno			;starting from the beginning

.searchcyc	ldy	#0
		lda	(z_tmp),y		;TypeString length
		bmi	.unknown
		jsr	checktypestr
		bcs	.found
		inc	.typno
		ldy	#0
		lda	(z_tmp),y		;TypeString length
		tay
		iny
		tya
		clc
		adc	z_tmpl
		sta	z_tmpl
		bcc	.searchcyc
		inc	z_tmph
		bne	.searchcyc		;BRA
.found		ldx	.typno
		lda	deviceattrs,x
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		and	#%01000000		;TCBM?
		beq	.serialdevice
		dec	_ser_units		;If TCBM, no serial device, +1-1
  !if (BITFIRE_1551_ONLY8 != 0) {
		lda	z_fa			;Unit No
		cmp	#8			;TCBM on Unit #8?
		beq	.serialdevice		;If yes, okay
		lda	deviceattrs,x
		and	#%01111111		;No Unit #8, not supported
		bne	.serialdevice+3
  }
.serialdevice	lda	deviceattrs,x
}
		bpl	.unsupported
		sta	_lastunittype		;Last checked and supported unit type
		ldx	z_fa			;Unit No
		stx	_lastunitno		;Last checked and supported unit No
.unsupported	rts
.unknown	inc	_prn_units		;Count unknown units
		lda	#%00000000+bus_unknown	;Unidentified
		rts
.typno		!byte	0

checktypestr	lda	#statlen
		sec
		sbc	(z_tmp),y		;Max length
		sta	.length
		ldx	#0
.nextbegin	ldy	#0
		lda	(z_tmp),y		;Read length
		tay
.nextpos	lda	(z_tmp),y
		cmp	_statusstring,x
		stx	.xsave+1		;X save
		beq	.found1
		inx
		cpx	.length
		bne	.nextpos
		clc				;CLC: not found
		rts
.found1		inx
		dey
		bne	.nextpos2
		sec				;SEC: found
		rts
.nextpos2	lda	(z_tmp),y
		cmp	_statusstring,x
		beq	.found1
.xsave		ldx	#$ff			;<- Modified
		inx
		cpx	.length
		bne	.nextbegin
		clc				;CLC: not found
		rts
.length		!byte	0

;	Drive identifier strings
;	One byte string's length, followed id string backwards
devicestrings	!byte	4
		!text	"1451"		;"1541"
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		!byte	5
		!text	"ksidt"		;"tdisk"
}
		!byte	4
		!text	"1751"		;"1571"
		!byte	4
		!text	"1851"		;"1581"
		!byte	6
		!text	"cei2ds"	;"sd2iec"
		!byte	$ff		;Device strings end

;	Drive attributes:
;	B7=1: Maybe supported (Selectable)
;	B6=1: TCBM / no serial unit
;	B5=1: No serial unit limit
;	B3210: Device type
deviceattrs
!if (BF_DRV_1541 = 1) {
		!byte	%10000000+bus_1541	;1541, cbmser
} else {
		!byte	%00000000+bus_1541	;1541, cbmser
}
!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
  !if (BF_DRV_1551 = 1) {
		!byte	%11100000+bus_1551	;1551
  } else {
		!byte	%01100000+bus_1551	;1551
  }
}
!if (BF_DRV_1541 = 1) {
		!byte	%10000000+bus_1571	;1571
} else {
		!byte	%00000000+bus_1571	;1571
}
!if (BF_DRV_1581 = 1) {
		!byte	%10000000+bus_1581	;1581
} else {
		!byte	%00000000+bus_1581	;1581
}
		!byte	%00000000+bus_sd2iec	;SD2IEC ('Unsupported' first, VCPU support check later)
;		!byte	%00000000+bus_unknown	;Unidentified


;;;	"73,CBM DOS V2.6 1541,00,00"			<- 1541
;;;	"73,JIFFYDOS 5.0 1541,00,00"			<- 1541
;;;	"73,CBM DOS V2.6 TDISK,00,00"			<- 1551
;;;	"73,CBM DOS V3.0 1571,00,00"			<- 1571
;;;	"73,COPYRIGHT CBM DOS V10 1581,00,00"		<- 1581
;;;	"73,(C) 1989 JIFFYDOS 6.0 1581,00,00"		<- 1581
;;;	"73,SD2IEC V1.1.0RC3,00,00"			<- SD2IEC

;	SD2IEC VCPU support check:
!if (BF_DRV_SD2IEC = 1) {
checkvcpusupport
		lda	#$6f
		jsr	opentolisten
		lda	#'z'			;Send "ZI"
		jsr	iecout
		lda	#'i'
		jsr	iecout
		jsr	unlisten		;"ZI": Get VCPU version informations
		jsr	readanswerfromdrive	;Read back the answer
!if (BITFIRE_FSDV_BITS = 1) {
		lda	#%00100000+bus_sd2iec	;SD2IEC, no VCPU support, not supported drive
} else {
		lda	#%00000000+bus_sd2iec	;SD2IEC, no VCPU support, not supported drive
}
		ldx	_statusstring+0		;First BYTE from status string (maybe $41/$42, but not $33 (ascii code of "3"))
		cpx	#'3'			;The answer is "30,SYNTAX..."?
		beq	.novcpusupport
		ldx	_statusstring+4		;I/O area size
		beq	.novcpusupport		;No data in old firmware (this check can be omitted later)
  !if (BITFIRE_FSDV_40TRK = 1) {
		pha
		lda	_statusstring+0		;First BYTE from status string (maybe $41/$42)
		and	#%00011111		;Only VCPU revision code remain
		cmp	#2			;VCPU-R2(+) required for 40 track images
		pla
		bcc	.novcpusupport		;If R1, no 40 track support
  }
		ora	#%10000000		;SD2IEC with VCPU support, _supported_ drive
		sta	_lastunittype		;Last checked and supported unit type
		ldx	z_fa			;Unit No
		stx	_lastunitno		;Last checked and supported unit No
.novcpusupport	rts
}


_statusstring	!byte	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		!byte	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		!byte	0,0,0,0,0,0,0,0
statlen = * - _statusstring

_firstscan	!byte	0			;If 0: no scan run
_scanbreak	!byte	0			;If <>0: scanning not possible (overloaded bus?)
_ser_units	!byte	0			;Counted units on serial bus
_all_units	!byte	0			;Counted units (all)
_prn_units	!byte	0			;Printer/Plotter on serial bus
_lastunitno	!byte	0			;Last checked and supported unit No
_lastunittype	!byte	0			;Last checked and supported unit type
_loadfromunit	!byte	0			;Last used unit no (from RAM "FA" (KERNAL))

_devices
!if BITFIRE_UNITSCAN_PRN = 1 {
		!byte	0,0,0,0			;4..7 units types (Printer only)
}
		!byte	0,0,0,0,0,0,0,0		;8..15 units types
_devicetable_len = * - _devices


;	"Open" device to Listen:
;	A <- Secondary address
opentolisten	sta	z_sa
		lda	z_fa
		jsr	listen
		lda	z_sa
		jmp	listen_sa
