!src "../config/config.inc"
!convtab pet
!cpu 6510

;XXX TODO have sector loading in ZP instead of all this?
;XXX TODO load of $80 does a loadnext and autoinc own counter that start with 0 after each load_dir -> thus it is reset after change disk \o/
;XXX TODO sector readahead -> after load, loadnext -> wishlist an all populated, load one sector and do not transfer but return. then compare new filenum if not load_next -> on equal, go directly with send:block, else clear wishlist and continue with other file

;--------------- ZP usage ----------------
;.. = unused
;bl = blocks on list
;ms = max sectors on current track
;fs = filesize in blocks (256 byte blocks)
;tr = current track
;in = number of currently loaded block
;pl = preamble bytes lo nibbles
;ph = preamble bytes hi nibbles
;sh = sector header data (gcr encoded -> 5 lo- and hinibbles)
;ds = dirsect
;bs = blocksize, size of transferred block
;fn = filenumber

;00: ff ff ff ff ff ff ff ff <-+
;08: ff 0e 0f 07 ff 0a 0b 03 <-+
;10: ff ff 0d 05 ff 00 09 01 <-| gcr2ser table with gaps being used by other variables
;18: ff 06 0c 04 ff 02 08 .. <-+ <- same as bin2ser lookup table
;20: .. ff ff xx xx xx xx xx <-+
;28: xx xx xx xx xx xx xx xx <-| list of wanted sectors	;XXX here still gaps can be zero
;30: xx xx xx xx xx xx xx xx <-+
;38: fs ff ff pl pl pl pl pl
;40: pl ff ff tm .. .. .. ..
;48: 0e 08 80 sh sh sh sh sh
;50: 0f 00 00 sh sh sh sh sh
;58: 07 01 10 fi fi fi fi fi
;60: fi ff ff .. .. .. .. ..		;filestruct
;68: 0a 0c c0 bs lb it ba fn
;70: 0b 04 40 .. .. .. .. ..
;78: 03 05 50 .. .. .. .. ..
;80: 0f 07 0d 05 0b 03 09 01 <-+ bin2ser lookup table
;88: 0e 06 0c 04 0a 02 08 00 <-+
;90: 0d 02 20 .. .. .. .. ..
;98: 05 03 30 ph ph ph ph ph
;a0: ph ff ff bl tr ms ds in
;a8: 00 0f f0 .. .. .. .. ..
;b0: 09 06 60 .. .. .. .. ..
;b8: 01 07 70 .. .. .. .. ..
;c0: .. ff ff .. .. .. .. ..
;c8: 06 09 90 .. .. .. .. ..
;d0: 0c 0a a0 .. .. .. .. ..
;d8: 04 0b b0 .. .. .. .. ..
;e0: .. ff ff .. .. .. .. ..
;e8: 02 0d d0 .. .. .. .. ..
;f0: 08 0e e0 .. .. .. .. ..
;f8: ff ff ff ff ff ff ff ff
;    ^  ^  ^
;    |  |  |
;    |  |  gcr dec_hi with index << 3
;    |  gcr_dec_lo with index << 3
;    gcr2ser with index << 3
;
;thus it is sufficient to inflate the gcr values in that way that they are either right or left aligned
;-----------------------------------------

STEPPING_SPEED	= $98

_drvcode_addr	= $0000

blocks_on_list	= $a3		;blocks tagged on wanted list
track		= $a4		;current track
max_sectors	= $a5		;maximum sectors on current track
dirsect		= $a6
index		= $a7		;current blockindex
blocks		= $38		;number of blocks the file occupies
blocksize	= $67

file_descriptor	= $5b
to_track	= file_descriptor + 0
sector		= file_descriptor + 1
ld_addr		= file_descriptor + 2
file_size	= file_descriptor + 4

temp		= $43
firstblock	= $44
cmp1		= sector
cmp2		= index
;dest		= $52		;-> 52 is always zero for free $53 will be set, sectorheader is unused during upload

preamble_lo	= $3b		;preamble data gcr coded lonibbles
preamble_hi	= $9b		;preamble data gcr coded hinibbles

dst		= preamble_lo

wanted		= $23		;list of wanted sectors/blockindices -> $15 bytes space

head_lo		= $4b		;sector header gcr data lonibbles
head_hi		= $53		;sector header gcr data hinibbles

;indexts	= $6d
;barrier	= $6e
filenum 	= $6f

  !if (BF_DRIVE = 1541) {				;===== 1541
gcr2ser		= $00		;gcr to serial-port lookuptablea -> from $08-$1f
bin2ser		= $80		;binary to serial data
  } else {						;===== 1551
z_chksum	= $02
z_gcrdecode	= $03		;4 bytes, GCR -> BIN decoder
z_blocklen	= $08
  }


directory	= $0500		;directory
lonibbles	= $0600
hinibbles	= $0700

  !if (BF_DRIVE = 1541) {				;===== 1541
gcr_dec_lo	= $f8c0
gcr_dec_hi	= $f8a0
gcr_dec_lo_shf	= $01
gcr_dec_hi_shf	= $02
  } else {						;===== 1551
gcrbin_98765xxx	=	$f70c	;/F70C/ <- "GCR-98765... > B-7654...." table (256 BYTEs, all 8th. BYTE valid)
gcrbin_10xxx432	=	$f70c	;/F70C/ <- "GCR-10...432 > B-....3210" table (256 BYTEs, 4 * 8 BYTEs)
gcrbin_xx98765x	=	$f70c	;/F70C/ <- "GCR-..98765. > B-7654...." table (64 BYTEs, even BYTEs valid)
gcrbin_3210xxx4	=	$f70e	;/F70E/ <- "GCR-3210...4 > B-....3210" table (256 BYTEs, 16 pcs. 2 BYTEs block)
gcrbin_5xxx9876	=	$f71c	;/F71C/ <- "GCR-5...9876 > B-7654...." table (256 BYTEs, 2 pcs. 16 BYTEs block)
gcrbin_x43210xx	=	$f70d	;/F70D/ <- "GCR-.43210.. > B-....3210" table (128 BYTEs, all 4th. BYTE valid)
gcrbin_765xxx98	=	$f729	;/F729/ <- "GCR-765...98 > B-7654...." table (256 BYTEs, 8 pcs. 4 BYTEs block)
gcrbin_xxx43210	=	$f6ff	;/F6FF/ <- "GCR-...43210 > B-....3210" table (32 BYTEs, cont.)
  }

DIR_SECT	= 18

!if (BF_DRIVE = 1541) {				;===== 1541
BUSY		= $02
BLOCK_READY	= $08
IDLE		= $00

VIA2_LED_OFF	=	$f7
VIA2_LED_ON	=	$08
VIA2_MOTOR_OFF	=	$fb
VIA2_MOTOR_ON	=	$04
} else {						;===== 1551
BUSY		= %01111111
BLOCK_READY	= %10111111
IDLE		= %11111111

TCPU_LED_OFF	=	%00001000
TCPU_LED_ON	=	%11110111
TCPU_MOTOR_OFF	=	%11111011
TCPU_MOTOR_ON	=	%00000100
}

	* = _drvcode_addr

drivecode_start

		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff	;$00
		!byte $ff, $0e, $0f, $07, $ff, $0a, $0b, $03 ;<-+
		!byte $ff, $ff, $0d, $05, $ff, $00, $09, $01 ;<-|$10  gcr2ser table with gaps being used by other variables
		!byte $ff, $06, $0c, $04, $ff, $02, $08, $ff ;<-+
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff	;$20
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff	;$30
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff	;$40
		!byte $0e, $08, $80, $ff, $ff, $ff, $ff, $ff
		!byte $0f, $00, $00, $ff, $ff, $ff, $ff, $ff	;$50
		!byte $07, $01, $10, $12, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff	;$60
		!byte $0a, $0c, $c0, $ff, $00, $00, $00, $00	;filenum
		!byte $0b, $04, $40, $ff, $ff, $ff, $ff, $ff	;$70
		!byte $03, $05, $50, $ff, $ff, $ff, $ff, $ff
		!byte $0f, $07, $0d, $05, $0b, $03, $09, $01 ;<-+$80 bin2ser lookup table
		!byte $0e, $06, $0c, $04, $0a, $02, $08, $00 ;<-+
		!byte $0d, $02, $20, $ff, $ff, $ff, $ff, $ff	;$90
		!byte $05, $03, $30, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $00, $12, $ff, DIR_SECT, $ff	;$a0
		!byte $00, $0f, $f0, $ff, $ff, $ff, $ff, $ff
		!byte $09, $06, $60, $ff, $ff, $ff, $ff, $ff	;$b0
		!byte $01, $07, $70, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff	;$c0
		!byte $06, $09, $90, $ff, $ff, $ff, $ff, $ff
		!byte $0c, $0a, $a0, $ff, $ff, $ff, $ff, $ff	;$d0
		!byte $04, $0b, $b0, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff	;$e0
		!byte $02, $0d, $d0, $ff, $ff, $ff, $ff, $ff
		!byte $08, $0e, $e0, $ff, $ff, $ff, $ff, $ff	;$f0
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff

		;leave 8 byte of stack, enough

		* = $108
		;-------------------------------------------------------------------------------------------------------------------------------------------------------------
		;NOTE TO SELF: DO NEVER TOUCH THAT SECTOR READ CODE AGAIN, FOR FUCKINGS SAKE! DO NOT TRY TO SAVE BYTES HERE, IT WILL BREAK TESTS WITH THCM'S SLOPPY FLOPPY :-D
		;-------------------------------------------------------------------------------------------------------------------------------------------------------------
drivecode_stack

  !if (BF_DRIVE = 1551) {				;===== 1551 only routines

;	Combined "Sector Head" / "Sector Data" read routine
;	This routine read the GCR stream from disk. Decode
;	  to binary, and calculate checksum in "read-time",
;	  on-the-fly. No any post-processing needed.
;	The algo inspired by original 1551 DOS, and use the
;	  ROM's GCR decoding tables.

readdisk_1551	sta	.rd51_markcmp+1

.rd51_markwait	bit	$4002			;B6 = SYNC bit
		bvs	*-3			;Wait for _ACTIVE_ sync
		bit	$4001			;Clear DV bit
		bit	$4001			;Clear DV bit
		bit	$01
		bpl	*-2			;Wait Byte Ready, first byte after sync
		lda	$4001			;AAAAAaaa	must be $55/$52
.rd51_markcmp	cmp	#$00			;self-modified - block header or block data?
		bne	.rd51_markwait
		bit	$4001			;Clear DV bit
		bit	$4001			;Clear DV bit (wait...)

		bit	$01
		bpl	*-2
		lax	$4001			;aaBBBBBb Type / B000 G98765, B000 G4
		and	#%00000001		;B000 G4
		sta	z_gcrdecode+1
		txa
		and	#%00111110		;B000 G98765
		sta	z_gcrdecode+0
		lda	#$00
		sta	z_chksum		;(26)



		bit	$01			;#0
		bpl	*-2
		lax	$4001			;bbbbCCCC B000 G3210, B001 G9876
.rd51_rdcycle	and	#%00001111		;B001 G9876
		sta	z_gcrdecode+2
		txa
		and	#%11110000		;B000 G3210
		ora	z_gcrdecode+1		;B000 G3210...4
		tax
		lda	gcrbin_3210xxx4,x	;B000 G43210 -> B3210
		ldx	z_gcrdecode+0
		ora	gcrbin_xx98765x,x	;B000 G98765 -> B7654
		sta	lonibbles,y		;(41/51)

		bit	$01			;#1
		bpl	*-2
		lax	$4001			;CcccccDD B001 G5, B001 G43210, B002 G98
		and	#%00000011
		sta	z_gcrdecode+1
		txa
		and	#%10000000
		ora	z_gcrdecode+2
		sta	z_gcrdecode+2
		txa
		and	#%01111100		;B001 G43210
		tax
		lda	gcrbin_x43210xx,x	;B001 G43210 -> B3210
		ldx	z_gcrdecode+2
		ora	gcrbin_5xxx9876,x	;B001 G98765 -> B7654
		sta	lonibbles+1,y		;(48)

		bit	$01			;#2
		bpl	*-2
		lax	$4001			;DDDddddd B002 G765, B002 G43210
		and	#%11100000
		ora	z_gcrdecode+1
		sta	z_gcrdecode+1
		txa
		and	#%00011111
		tax
		lda	gcrbin_xxx43210,x	;B002 G43210 -> B3210
		ldx	z_gcrdecode+1
		ora	gcrbin_765xxx98,x	;B002 G98765 -> B7654
		sta	lonibbles+2,y		;(41)

		bit	$01			;#3
		bpl	*-2
		lax	$4001			;EEEEEeee B003 G98765, B003 G432
		and	#%00000111
		sta	z_gcrdecode+3
		txa
		and	#%11111000
		sta	z_gcrdecode+2
		lda	z_chksum
		eor	lonibbles,y
		iny
		eor	lonibbles,y
		iny
		eor	lonibbles,y
		iny
		sta	z_chksum		;(45)

		bit	$01			;#4
		bpl	*-2
		lax	$4001			;eeFFFFFf B003 G10, B004 G98765, B004 G4
		and	#%00111110
		sta	z_gcrdecode+0
		txa
		and	#%00000001
		sta	z_gcrdecode+1
		txa
		and	#%11000000
		ora	z_gcrdecode+3
		tax
		lda	gcrbin_10xxx432,x	;B003 G43210 -> B3210
		ldx	z_gcrdecode+2
		ora	gcrbin_98765xxx,x	;B003 G98765 -> B7654
		sta	lonibbles,y
		eor	z_chksum		;(51) (total: 236 cycles / 40 GCR bits)

		bit	$01			;#5/#0
		bpl	*-2
		sta	z_chksum
		lax	$4001			;ffffGGGG B004 G3210, B005 G9876
		iny
		beq	.rd51_rdlast
		jmp	.rd51_rdcycle		;(19+32)



.rd51_rdlast	and	#%11110000
		ora	z_gcrdecode+1
		tax
		lda	gcrbin_3210xxx4,x	;B004 G43210 -> B3210
		ldx	z_gcrdecode+0
		ora	gcrbin_xx98765x,x	;B000 G98765 -> B7654
		eor	z_chksum
		rts

  }


read_sector	;need to read sector header beforehand to compare with our wanted list, if we would seek a certain sector, the kernal routines would do
;		lda #$00
;		sta $1801
.read_sector_
  !if (BF_DRIVE = 1541) {				;===== 1541
		jsr wait_sync_mark
		bvc *
		lda $1c01		;22333334
		clv
					;11111 222	header type
		cpx #$52
		bne .read_sector_
		;lda $1c01		;22333334	type  / checksum
		asr #%00111111
		sta head_hi+1		;store @ pos 1						;7
.chksum
		bvc *

		lax $1c01		;44445555	checksum / sector
		ror			;lots of shifting, we could of course also work on left aligned values, but would then need a double lookup when checksumming :-(
		lsr
		lsr
		lsr
		sta head_lo+1

		clv
		bvc *

		txa
		ldx $1c01		;56666677	sector / track
		cpx #$80
		rol
		and #%00011111
		sta head_hi+0		;store @ pos 0 instead of pos 1 to reuse value later on

		clv
		bvc *

		txa
		asr #%01111111		;bit 4 of 7 in carry
		sta head_lo+0		;1 bit of 7 left
		lda $1c01		;77788888	track
		clv
		ror			;-> 77778888
		bvc *

		lsr head_lo+0		;fetch msb of 7


		;skip loading track information, we have it at hand and let checksum decide if we are on right track, it will fail on wrong track anyway, so we have our track check for free

;		ror			;-> 77777888
;		lsr
;		lsr
;		lsr
;		lda head_hi+2		;-> ---77777
;		lda #%00011111
;		lax head_lo+2		;-> ---88888
				;read after 40 cycles
		ldx #$07
		lda $1c01		;11111222	id2
		sax head_lo+2

		clv
		bvc *

		lsr
		lsr
		lsr
		sta head_hi+2		;---11111

		lax $1c01		;22333334	id2/id1
		asl
		rol head_lo+2
		asl
		rol head_lo+2
		txa
		asr #%00111111
				;read on cycle 40
		sta head_hi+3

		lda $1c01		;44445555	id2
!if >*-1 != >.chksum { !error "header-checksum not in one page: ", .chksum, " - ", * }
		ror
		lsr
		lsr
		lsr
		sta head_lo+3

		ldx #$03
-
		ldy head_hi,x		;decode sector header
		lda gcr_dec_hi,y
		ldy head_lo,x
		ora gcr_dec_lo,y
		sta head_lo,x
		dex
		bpl -
		tax			;sector number

		eor head_lo + 1		;build header checksum A = .head_lo + 0
		eor head_lo + 2
		eor head_lo + 3
		eor track		;no need to fetch/check track, checksum will fail on wrong track
.rs_back
		bne .read_sector_	;urgh, bad checksum, find another sector
  } else {						;===== 1551
		lda	#$52			;Block header "sign"
		ldy	#252			;256-4: 4 BYTEs read
		jsr	readdisk_1551
		bne	.read_sector_		;Header Checksum error: find next
		lda	lonibbles+254		;Readed header: Track number
		cmp	track
		bne	.read_sector_		;Wrong track: read next sector (ugh...)
		ldx	lonibbles+255		;Readed header: ID1
		stx	head_lo+2
		ldx	z_chksum		;Readed header: ID2
		stx	head_lo+3
		ldx	lonibbles+253		;Readed header: Sector number
		stx	head_lo+0
  }

		;will be transformed into a lda .wanted,x
		ldy wanted,x		;sector on list?
		iny			;$ff + 1 = 0?
		;yes, read, we should still have ~20 cycles overhead here until next sync arrives
		bne *+5
.skip_wcheck	jmp .read_sector_

.wait_sector_data
;!if BITFIRE_CONFIG_IN_ORDER = 1 {	;no barriers needed with standalone loadraw
;		jmp .check
;.check_back
;}
  !if (BF_DRIVE = 1541) {				;===== 1541
		jsr wait_sync_mark
		bvc *
		lda $1c01		;22333334
		clv
		cpx #$55
		bne .rs_back
		bvc *
		asr #%00111111
		ldy #$00		;waste 6 cycles to have more or less same entry point into loop, y = $ff due to sync mark?
		bit $ea

.load_sector_data
		sta hinibbles,y		;---33333 4
		lax $1c01		;44445555				;read @ 16 / 16 / 16 / 16
		arr #$f0		;44444---				;/!\ ATTENTION, arr potentially fucks the v flag status!
		sta lonibbles,y		;44444---
		txa			;44445555
		and #%00001111		;----5555
		iny
		sty .lindex+1		;avoid index on lsr

.smc1		bne *			;BRA, y is never 0, could also use bvs, but arr possibly influences V flag /o\
		nop			;so waste either 3,5,7,9 cycles depending on speedzone
		nop
		nop

		ldx $1c01		;56666677				;read @ 40 / 42 / 44 / 46
		cpx #$80		;copy 5 to carry
		rol
		sta hinibbles,y
		txa			;56666677
		asr #%01111111		;--666667
		sta lonibbles,y
		iny

.smc2		bne *
		nop
		nop
		nop

		lax $1c01		;77788888				;read @ 67 / 71 / 75 / 79
		and #%00011111
		sta lonibbles,y		;lda #$0f sax .lonibbles,y if ZP
		txa			;77788888 is fetched again
		ror
.lindex		lsr lonibbles		;shift out last 7 -> ---66666
		arr #$f0		;77777---
		sta hinibbles,y
		clv								;now finally clear v flag, as we are past the last arr operation
		lda $1c01		;11111222				;read @ 95 / 99 / 103 / 107
		lsr
				;XXX TODO store $22...222 as ll...hhh
		bvc *

		iny
		sta hinibbles,y		;-1111122 2
		lda $1c01		;22333334
		clv
		ror			;22233333 4
		sta lonibbles,y		;store for later processing		;120 cycles min

		bvc *

		and #%00011111		;---33333 4
		iny
		bne .load_sector_data
!if >*-1 != >.load_sector_data { !error "gcr-decoding not in one page: ", .load_sector_data, " - ", * }

		tay
		lda $1c01
		arr #$f0
		tax
		lda gcr_dec_hi,y	;XXX TODO would also work with gcr2ser table but would reduce reliability
		eor gcr_dec_lo_shf,x
		tay			;checksum

;		lda #$20
;		sta $1801

		ldx #$fc

		;finish gcr inflation and checksum the data
		;XXX TODO place code on zp and use smc instead of tay/tya.
		;place lo/hinibbles+3 on zp?
-
		lda lonibbles + 3,x	;4
		lsr hinibbles + 3,x	;7
		ror			;2
		lsr hinibbles + 3,x	;7
		arr #$f0		;2
		sta lonibbles + 3,x	;5

		tya
		ldy hinibbles + 0,x
		eor gcr_dec_hi,y
		ldy lonibbles + 0,x
		eor gcr_dec_lo_shf,y
		ldy hinibbles + 1,x
		eor gcr_dec_hi,y
		ldy lonibbles + 1,x
		eor gcr_dec_lo,y
		ldy hinibbles + 2,x
		eor gcr_dec_hi_shf,y
		ldy lonibbles + 2,x
		eor gcr_dec_lo,y
		ldy hinibbles + 3,x
		eor gcr_dec_hi,y
		ldy lonibbles + 3,x
		eor gcr_dec_lo_shf,y
		tay

		txa
		sbx #$04
		bcs -

		tya
		bne ++			;checksum okay?
  } else {						;===== 1551
		lda	#$55			;Block data "sign"
		ldy	#0			;256 BYTEs read
		jsr	readdisk_1551
		bne	++			;Data Checksum error: find next
		ldy	#$00
  }

		ldx head_lo + 0		;current sector number
		lda wanted,x		;grab index from list (A with index reused later on after this call)
		dey			;blocksize full sector ($ff)
		sty wanted,x		;clear entry in wanted list
		cmp file_size + 1	;as we count up in $100 blocks, if index equals highbyte of size, we have loaded last block
		bne +
		ldy file_size		;block size of last sector (lowbyte filesize)
+
		sty blocksize		;remember for later use
		rts			;done
++
		jmp read_sector


send_block
		ldy #$00		;start with 0, we will send 2 or 4 bytes of blockinfo as preamble
  !if (BF_DRIVE = 1541) {				;===== 1541
		ldx preamble_lo
		lda bin2ser,x
		;and #$05
		ora #BLOCK_READY | BUSY
		bne .presend_strt		;will send 6 valid bits and signal block ready

		;on atn going low we have alraedy first data on bus with the upcoming code
preloop
		lda bin2ser,x
		bit $1800
		bmi *-3

.presend_strt	sta $1800		;send first bits on end of atn strobe -> 4th sta on c64 side
		asl
		ora #$10
		ldx preamble_hi,y	;hinibbles are already shifted into right position

		bit $1800
		bpl *-3
		sta $1800		;second -> first sta $dd02 on c64 side
		lda bin2ser,x
		iny
		;nop

		bit $1800
		bmi *-3
		sta $1800		;third -> second sta $dd02 on c64 side
		asl
		ora #$10
		ldx preamble_lo,y

		bit $1800
		bpl *-3
		sta $1800		;last -> third sta $dd02 on c64 side
pre_cmp = * + 1
		cpy #$00
		bcc preloop
!if >*-1 != >preloop {
	!error "preloop not in one page! Overlapping bytes: ", * & 255
}
  } else {						;===== 1551
		lda	preamble_lo			;Read preamble BY0
		ora	#%11000000			;6 valid bits send to 1st. preamble BYTE
		and	#BLOCK_READY & BUSY
		tax					;READY to send
		iny
		lda	#%00000001
		stx	$4000				;BY0 to TCBMD
		ldx	preamble_lo,y			;Read preamble BY1
		iny
		bit	$4002
		beq	*-3
		stx	$4000				;BY1 to TCBMD


pre_cmp1 = * + 1
		cpy	#$00
		beq	preover
preloop		ldx	preamble_lo,y			;Read preamble BY2/4/6
		iny
		bit	$4002
		bne	*-3
		stx	$4000				;BY2/4/6 to TCBMD
		ldx	preamble_lo,y			;Read preamble BY3/5/7
		iny
		bit	$4002
		beq	*-3
		stx	$4000				;BY3/5/7 to TCBMD
pre_cmp2 = * + 1
		cpy	#$00
		bne	preloop				;
  }
preover


  !if (BF_DRIVE = 1541) {				;===== 1541
		ldy blocksize
		ldx lonibbles,y
		nop
.sendloop				;send the data block
		lda gcr2ser,x

		bit $1800		;18 + 7
		bmi *-3
		sta $1800		;read by first lda $dd00 on c64 side
		asl
		ora #$10
		ldx hinibbles,y

		bit $1800		;18 + 7
		bpl *-3
		sta $1800		;read by second lda $dd00 on c64 side
		lda gcr2ser,x
		dey
		;nop			;keep flow even? we can allow for one branch to hit in per round?

		bit $1800		;18 + 7
		bmi *-3
		sta $1800		;read by third lda $dd00 on c64 side
		asl
		ora #$10
		ldx lonibbles,y

		bit $1800		;19 + 7
		bpl *-3
		sta $1800		;read by fourth lda $dd00 on c64 side
		cpy #$ff
		bcc .sendloop

		;XXX carry is set here, always, might be useful somewhen
		lda #BUSY
		bit $1800
		bmi *-3
		sta $1800
		rts
  } else {						;===== 1551
		ldy	z_blocklen
		ldx	lonibbles,y
		bit	$4002
		bne	*-3
		stx	$4000
		dey
		beq	.sendloop_end
.sendloop	ldx	lonibbles,y		;
		bit	$4002
		beq	*-3
		stx	$4000
		dey
		ldx	lonibbles,y
		bit	$4002
		bne	*-3
		stx	$4000
		dey
		bne	.sendloop		;(35)

.sendloop_end	ldx	lonibbles,y
		bit	$4002
		beq	*-3
		stx	$4000
		ldx	#BUSY
		bit	$4002
		bne	*-3
		stx	$4000
		dey
		sec
		rts
  }
!if >*-1 != >.sendloop {
	!error "sendloop not in one page! Overlapping bytes: ", * & 255
}

;!if BITFIRE_DECOMP = 1 {		;no barriers needed with standalone loadraw
;.check
;!if BITFIRE_CONFIG_IN_ORDER = 1 {
;		tya			;y = block index + 1
;		clc			;subtract 1 too much
;		sbc barrier		;distance
;		cmp #$01		;only allow distance of 0 (first block) or 1
;		bcs +
;		jmp .check_back
;+
;		jmp .read_sector_
;} else {
;		;force 1. sector of file to be loaded first
;;		lda barrier
;;		bne +
;;		dey			;fetch blockindex
;;		bne ++
;;+
;;		jmp .check_back
;;++
;;		jmp .read_sector_
;}
;}

preamble_build
		pha			;block number is in A, save

		ldy #$00

		lda firstblock
		beq +
		inc firstblock
		jsr .preamble_add_byte	;$ff as ack byte

  !if (BF_DRIVE = 1551) {				;===== 1551
!if (((BITFIRE_DEBUG = 1) and (BITFIRE_DECOMP = 1)) or ((BITFIRE_DEBUG = 0) and (BITFIRE_DECOMP = 0))) {
		jsr .preamble_add_byte		;If 1551: add dummy to preamble, preamble length is always even no. of bytes
}
  }

!if BITFIRE_DEBUG = 1 {
		lda filenum
		jsr .preamble_add_byte
}
		lda ld_addr
		jsr .preamble_add_byte
		lda ld_addr+1
+
		jsr .preamble_add_byte	;$00 as ack byte / ld_addr+1

!if BITFIRE_DECOMP = 1 {		;no barriers needed with standalone loadraw
		lda index		;max index to match against
		ldx #$14		;walk through list of sectors to be loaded
-
		cmp wanted,x		;compare
		bcc +			;bigger index, next please
		lda wanted,x		;smaller (or same, but can't happen, as index is unique) remember new minimum
+
		dex			;next entry
		bpl -

;!if BITFIRE_CONFIG_IN_ORDER = 1 {
;		sta barrier		;save new barrier
;}
		clc
		adc ld_addr+1
		jsr .preamble_add_byte	;barrier, absolute
}
		pla
		clc
		adc ld_addr+1		;add load_addr hibyte to block number to make the block number an absolute address on c64 side
		jsr .preamble_add_byte	;block_addr_hi

		ldx blocksize		;set up num of bytes to be transferred
  !if (BF_DRIVE = 1551) {				;===== 1551
		stx	z_blocklen
		txa
		lsr
		bcs	+
		inc	z_blocklen	;If block length is odd number of bytes, add dummy
+
  }
		inx			;increase by one, as it is decreased before receiving first byte on c64
		txa

.preamble_add_byte
  !if (BF_DRIVE = 1541) {				;===== 1541
		ldx #$0f
		sax preamble_lo,y	;mask out lower 4 bits
		lsr
		lsr
		lsr
		lsr
		sta preamble_hi,y	;upper 4 bits
  } else {						;===== 1551
		sta	preamble_lo,y
  }
		iny
		rts

  !if (BF_DRIVE = 1541) {				;===== 1541
wait_sync_mark
		;XXX TODO maybe first check can be omitted? But this way we are save to not directly fall through next check if we missed the sync. We would read chunk then and possibly still go through the checksum, as it is just a simple 8 bit eor
;		lda $1c00		;wait for start of sync
;		bpl *-3
		lda $1c00		;wait for end of sync
		bmi *-3
		ldy $1c01		;sync mark -> $ff
		clv
		bvc *
		ldx $1c01		;11111222	must be $55/$52
		clv
		rts

!if >*-1 != >wait_sync_mark { !error "wait_sync_mark not in one page: ", wait_sync_mark, " - ", * }
  }



turn_disc
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda #VIA2_MOTOR_ON
  } else {						;===== 1551
		lda	#(TCPU_LED_OFF | TCPU_LED_ON)	;%11111111 :)
  }
		jsr motor_on
.turn_disc_
		jsr read_dir_sect0	;fetch first dir sector
		lda filenum
		sec
		sbc directory + $ff	;compare side info
		bne .turn_disc_		;nope, not the requested side
		sta filenum		;reset filenum
		;top
		beq	drivecode_launch

idle
		inc filenum		;autoinc always, so thet load_next will also load next file after a load with filenum
		dec firstblock

drivecode_launch
		;XXX TODO here it would be possible to preload next block

!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		jmp	.pull_command		;Mode 2: No automatic motor stop + led off
}

stopmotor
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda $1c00
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 0 | BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		and #(VIA2_LED_OFF & VIA2_MOTOR_OFF)
} else {
		and #VIA2_LED_OFF
}
		sta $1c00
  } else {						;===== 1551
		lda	$01
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 0 | BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		and	#TCPU_MOTOR_OFF
}
		ora	#TCPU_LED_OFF
		sta	$01
  }
.pull_command	jsr get_byte

		;load file, file number is in A
.load_file
		cmp #BITFIRE_UPLOAD
		bne *+5
		jmp upload
  !if (BF_DRIVE = 1551) {				;===== 1551
		tax
		lda	$4000		;TCBM input
		eor	#$ff		;=$FF? == input on plus/4 side?
		bne	*-5
		lda	#BUSY
		sta	$4000		;set status to BUSY
		lda	$4002
		ora	#%00001000	;ACK=1: TCBM DATA output on 1551 side
		sta	$4002
		lda	#$ff
		sta	$4003		;and set TCBM DATA port to output
		txa
  }

!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		cmp	#BITFIRE_STOPMOTOR	;STOP motor?
		beq	stopmotor
}
		cmp #BITFIRE_LOAD_NEXT
		beq .load_next
		sta filenum		;set new filenum
		bcs turn_disc
.load_next
		;XXX TODO send out preloaded sector here if filenum = expected filenum
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda #VIA2_LED_ON | VIA2_MOTOR_ON
  } else {						;===== 1551
		lda	#TCPU_LED_ON
  }
		jsr motor_on		;turn on led and motor, if not on already

		lda filenum		;get current or autoinced filenum
		ldx #DIR_SECT+1
		sec
-
		sta temp		;remmeber previous A
		dex
		sbc #42
		bcs -
+
		cpx dirsect		;dirsect changed?
		beq +			;nope, advance

		jsr read_dir_sect	;fetch next dir_sect
+
		lda temp		;fetch filenum % 42
					;file number * 6 to get proper index -> (num * 2 + num) * 2
		asl			;*2
		;clc			;can be omitted, not more than 42 files are allowed per dirsect, so carry is always cleared after asl
		adc temp		;+1 -> * 3
		asl			;*2 -> * 6
		tay

		ldx #$00		;reset block index
		stx index
;!if BITFIRE_DECOMP = 1 {		;no barriers needed with standalone loadraw
;!if BITFIRE_CONFIG_IN_ORDER = 1 {
;		stx barrier
;}
;}
-					;copy over direntry to ZP
		lda directory,y
		sta file_descriptor,x
		iny
		inx
		cpx #$06
		bne -
					;a = file_size highbyte, carry set
		adc #$00		;need to add one, as also the last partial block counts as a full block. File size is one too less, so exceptions like $xx00 in length are no problem
		sta blocks
					;send load-address with first transferred block
.load_track
		lda blocks
					;XXX TODO preload 1. block of next file here?
		beq idle		;check if all blocks of file are loaded

		jsr seek		;sets max_sectors and bitrate depending on track
--					;now find corresponding offset for first sector by subtracting until remainder (== offset) remains
!if BITFIRE_CONFIG_INTERLEAVE = 4 {
		lax sector		;load startsector#/offset of current rev and keep a copy in X
		and #$03
		sta sector
} else {
		lax sector		;load startsector#/offset of current rev and keep a copy in X
		sec			;calc modulo of .sector and keep it in .sector, XXX on interleave 4 a and #$03 would suffice, but we offer a variable interleave /o\
-
		sta sector		;keep old value as new offset
		sbc #BITFIRE_CONFIG_INTERLEAVE	;subtract some #sectors
		bcs -			;until underflow
}

		;we have now the right offset to start at
-
		ldy index		;get index
		sty wanted,x		;write index into wantedlist
		inc index		;advance index
		inc blocks_on_list	;count number of blocks in list (per track num of blocks)
		dec blocks		;all blocks loaded?
		beq .load_wanted_blocks	;yep, end
		txa			;go for next block
		sbx #-BITFIRE_CONFIG_INTERLEAVE	;x += INTERLEAVE
		cpx max_sectors		;wrap around?
		bcc -			;nope
		lda #BITFIRE_CONFIG_INTERLEAVE	;handle next round by increasing offset stored in .sector
		isc sector		;inc sector offset and compare with INTERLEAVE
		bne --			;on zero, all done, else next round
					;XXX TODO here still blocks need to be loaded
.load_wanted_blocks			;read and transfer all blocks on wishlist
		jsr read_sector		;returns with current blockindex in A
		jsr preamble_build	;add blockindex and size to preamble, and handle barrier stuff there
  !if (BF_DRIVE = 1541) {				;===== 1541
		sty pre_cmp		;set preamble size
  } else {						;===== 1551
		sty pre_cmp1		;set preamble size
		sty pre_cmp2		;set preamble size
  }
		jsr send_block		;exits with y = $ff and carry set
		dec blocks_on_list	;last block on wishlist?
		bne .load_wanted_blocks

;.track_finished
		iny
		sty sector		;set start pos for next track, y is always $ff after .send_block
-					;now adjust to_track and take care of skipping track 18
		lda #18
		;sec			;set by send_block and also set if beq
		isc to_track
		beq -			;skip dirtrack however
		bne .load_track		;BRA

seek
;		lda #$00		;switch to two Mhz on 1571
;		sta $1801
;.set_bitrate				;sets bitrate for desired track and also sets value for max. sectors on track
		ldy to_track

		ldx #$11		;max sectors on track
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda $1c00
  } else {						;===== 1551
		lda	$01
  }
		and #$9f
		cpy #31
		bcs ++			;-> bitrate = $00
		inx
		cpy #25
		bcs +			;-> bitrate = $20
		inx
		ora #$40
		cpy #18
		bcs ++			;-> bitrate = $40
		inx			;-> bitrate = $60
		inx
+
		ora #$20
++
		stx max_sectors		;store new max sectors
  !if (BF_DRIVE = 1541) {				;===== 1541
		sta $1c00		;now set bitrate

		asr #$60		;take four bitrates and shift down
		lsr
		lsr
		lsr
		lsr			;0/1/2/3
		sta .smc1+1		;adapt gcr decode to speedzone by modifying branches
		sta .smc2+1
  } else {						;===== 1551
		sta	$01		;now set bitrate
  }
		tya
		sec
		sbc track		;how many tracks to go?
		sty track		;save target track as current track

		ldy #$00		;make stepping positive
		bcs .seek_up		;up or downwards?
.seek_down
		eor #$ff		;down we go, so invert num tracks as it is negative
		adc #$01
		iny			;but make stepping negative
.seek_up
		asl			;counter is twice the number of tracks (halftracks)
		tax
		beq +			;nothing to step, end
.step
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda #STEPPING_SPEED
		sta $1c05
.halftrack
		tya
		eor $1c00
		sec
		rol
		and #3
		eor $1c00
		sta $1c00
		bit $1c05
		bmi *-3
  } else {						;===== 1551
;.halftrack
		tya
		eor	$01
		sec
		rol
		and	#%00000011
		eor	$01
		sta	$01
		stx	.stepsavex+1
		sty	.stepsavey+1	;Save X/Y
		ldx	#STEPPING_SPEED
--		ldy	#(102-1)	;2
-		dey			;2
		bne	-		;3 (5 cycles, if counting. 102*5 = 510, +2 = 512 clock-cycles = 256 1541 cycles.)
		dex
		bmi	--		;+5 cycles, if "big" cycle is counting. (102-1)
.stepsavex	ldx	#$00
.stepsavey	ldy	#$00		;Restore X/Y
  }
		dex
		bne .step
+
		rts

read_dir_sect0
		ldx #DIR_SECT
read_dir_sect
		stx dirsect
		inc wanted,x		;mark desired dir sector in wishlist

		lda #$12		;set target track to 18
		sta to_track
		;sta file_size + 1	;write any number > 0 to file_size + 1 to trigger a sector_length of 256 bytes

		jsr seek
		jsr read_sector

  !if (BF_DRIVE = 1541) {				;===== 1541
		ldy #$00
		;decode dir
-
		ldx hinibbles,y		;read either from shifted or real table
		lda gcr2ser,x		;load fitting serial entry from table, this can handle shifted gcr values as we have 2 combined tables in one
		tax
		lda bin2ser,x		;get corresponding binary value
		asl
		asl
		asl
		asl
		pha

		ldx lonibbles,y
		lda gcr2ser,x
		tax
		pla
		ora bin2ser,x
		sta directory,y
		dey
		bne -
  } else {						;===== 1551
		ldy	#$00
-		lda	lonibbles,y
		sta	directory,y
		dey
		bne	-
  }
		rts

motor_on
  !if (BF_DRIVE = 1541) {				;===== 1541
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		ldx	$1c00
}
		ora $1c00
		sta $1c00
  } else {						;===== 1551
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		ldx	$01
}
		and	$01			;LED ON/OFF
		ora	#TCPU_MOTOR_ON		;Motor ON
		sta	$01
  }
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		txa
  !if (BF_DRIVE = 1541) {				;===== 1541
		and	#VIA2_MOTOR_ON
  } else {
		and	#TCPU_MOTOR_ON
  }
		bne	.motorison
}

!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 0 | BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		inc .skip_wcheck	;disable wanted check ($4c of jmp is transformed into $4d = eor $xxxx), so any sector is okay
		jsr read_sector
		jsr read_sector
		jsr read_sector
		dec .skip_wcheck	;reenable check
.motorison
}
		rts

  !if (BF_DRIVE = 1541) {				;===== 1541
lock
-
		ldx $1800		;still locked?
		bmi -
get_byte
		ldy #BUSY		;enough time for signal to settle
.get_byte_
		lda #$80		;execpt a whole new byte
		sta $1800

		;ldx #$00		;start with a free bus
		ldx	#%00000100-1	;CLK line
-		cpx	$1800		;CLK line active?
		bcc	+		;if yes, transfer was started
		bcs	-

.gloop
-
		cpx $1800		;wait for transition
		beq -
+		ldx $1800		;reread register
		bmi lock		;is the bus locked via ATN? if so, wait
		cpx #$05		;nope, interpret bit
		ror			;and shift in
-
		cpx $1800		;wait for next transition
		beq -
		ldx $1800		;reread register
		bmi lock		;is the bus locked? XXX TODO can be omitted?
		cpx #$01		;nope, interpret bit
		ror			;and shift in

		bcc .gloop		;more bits to fetch?
		sty $1800		;set busy bit
		rts
  } else {						;===== 1551
get_byte	lda	#$00
		sta	$4003		;TCBM data to input
		lda	$4002
		and	#%00010111	;ACK=0: TCBM DATA input on 1551 side
		sta	$4002
		lda	#%00000001
		bit	$4002		;DAV check
		beq	*-3
		ldx	$4000		;Read TCBM data
		bit	$4002		;DAV check
		bne	*-3
		txa
		rts
  }

		;receive end_address of code being uploaded
		;maximum is $0110 - $04ae, buffers at $0500,$0600,$0700 can be used
		;if original code is restored, be sure that get_byte is at the same address
upload
		;receive dest
		jsr get_byte
		sta dst
		jsr get_byte
		sta dst+1

		;num bytes
		jsr get_byte
		sta cmp1
		jsr get_byte
		sta cmp2
		;XXX todo: copy part from here on onto zp, so full ram can be used for upload
-
		jsr get_byte
		ldy #$00
		sta (dst),y		;lowbyte of dest is always 0

		lda #$ff
		dcp cmp1
		bne +
		dcp cmp2
		beq .start
+
		dcp dst
		bne -
		dec dst+1
		bcs -
.start
		jmp (dst)

;.blink
;		pha
;		sec
;		rol
;		sta dest
;
;--
;		lda $1c00
;		ora #VIA2_LED_ON
;		sta $1c00
;
;		ldx #$01
;		ldy #$08
;		bcc .short
;		ldx #$08
;		ldy #$01
;.short
;-
;		lda #$ff
;		sta $1c05
;		lda $1c05
;		bne *-3
;
;		dex
;		bne -
;
;		lda $1c00
;		and #VIA2_LED_OFF
;		sta $1c00
;
;-
;		lda #$ff
;		sta $1c05
;		lda $1c05
;		bne *-3
;
;		dey
;		bne -
;
;		clc
;		rol dest
;		bne --
;		pla

;		rts

;end

drivecode_end

;emit warnings only once
!if * > $0500 { !serious "Upload code is ", * - $0500, " bytes too big!" }

!ifdef drivecode_end {
	!warn $0500 - *, " bytes remaining for drivecode."
}


drivecode_size = drivecode_end - drivecode_start

  !if (BF_DRIVE = 1541) {				;===== 1541
drivecode_41_VIA2LED_ON	=	VIA2_LED_ON
drivecode_41_VIA2MTR_ON	=	VIA2_MOTOR_ON
drivecode_41_bin2ser	=	bin2ser
drivecode_dos_bin2gcr	=	$f77f
drivecode_41_pre_cmp	=	pre_cmp
drivecode_41_preover	=	preover
drivecode_41_presend_s	=	.presend_strt
drivecode_41_bin2ser	=	bin2ser
  } else {						;===== 1551
drivecode_51_TCPULED_ON	=	TCPU_LED_ON
drivecode_dos_bin2gcr	=	$f8bf
  }

drivecode_IDLE		=	IDLE
drivecode_BUSY		=	BUSY
drivecode_BLOCK_RDY	=	BLOCK_READY
drivecode_to_track	=	to_track
drivecode_sector	=	sector
drivecode_filenum	=	filenum
drivecode_dirsect	=	dirsect
drivecode_head_lo	=	head_lo
drivecode_preamb_lo	=	preamble_lo
drivecode_preamb_hi	=	preamble_hi
drivecode_pullcomm	=	.pull_command
drivecode_motoron	=	motor_on
drivecode_getbyte	=	get_byte
drivecode_seek		=	seek
drivecode_stopmotor	=	stopmotor
drivecode_directory	=	directory
