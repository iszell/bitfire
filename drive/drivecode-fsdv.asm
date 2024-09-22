;	SD2IEC - FlexSD/VCPU drivecode
;
BF_DRIVE	=	101	;FlexSD

!src "vcpumacros-acme.asm"

!src "../config/config.inc"
!convtab raw
!cpu 6510

status_wpon	=	%11110011	;Write Protect On (B32 = %00)

!if (BITFIRE_FSDV_BITS = 1) {
  !macro setidle {
	+uchdh
  }
  !macro setbusy {
	+uchdl
  }
  !macro setready {
	+usckl
	+usdth
	+uwdth
	+uwdtl
  }
  !macro sendbyte {
	eor	#%11111111
	+usnd1
  }
  !macro waitsend {
	+uwckh
  }
} else {
  !macro setidle {
	+uchdh
  }
  !macro setbusy {
	+uchdl
  }
  !macro setready {
	;+usckl
  }
  !macro sendbyte {
	+usnd2
  }
  !macro waitsend {
	+uwath
  }
}

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
;-----------------------------------------

_drvcode_addr	= $0200

channelno	=	2

DIR_SECT	= 18

;BUSY		= $02		;<- DAT Low
;BLOCK_READY	= $08		;<- CLK Low
;IDLE		= $00		;<- DAT/CLK HiZ

BUSY_FSD	=	%11111101	;B1->DAT, B0->CLK : DAT Low
BLOCK_READY_FSD	=	%11111110	;B1->DAT, B0->CLK : CLK Low

	* = _drvcode_addr

drivecode_start

zeropage = *

blocks_on_list = * - zeropage
		!byte	$00		;$00 blocks tagged on wanted list
track = * - zeropage
		!byte	$12		;$01 current track
max_sectors = * - zeropage
		!byte	$ff		;$02 maximum sectors on current track
dirsect = * - zeropage
		!byte	$ee ;DIR_SECT	;$03 Actual Directory sector number, $EE: Force ReRead actual dir sector
index = * - zeropage
		!byte	$ff		;$04 current blockindex
blocks = * - zeropage
		!byte	$ff		;$05 number of blocks the file occupies
blocksize = * - zeropage
		!byte	$ff		;$06

file_descriptor = * - zeropage		;$07..0C
to_track	= file_descriptor + 0
sector		= file_descriptor + 1
ld_addr		= file_descriptor + 2		;Load address, 2 BYTE
file_size	= file_descriptor + 4		;File size, 2 BYTE
		!byte	$12,$ff,$ff,$ff,$ff,$ff

temp = * - zeropage
		!byte	$ff		;$0D
firstblock = * - zeropage
		!byte	$ff		;$0E
preamble_abs = *
preamble = * - zeropage
		!byte	$ff,$ff,$ff,$ff,$ff,$ff	;$0F..$14 preamble data, 6 BYTE
wanted = * - zeropage
		!byte	$ff,$ff,$ff,$ff,$ff,$ff
		!byte	$ff,$ff,$ff,$ff,$ff,$ff
		!byte	$ff,$ff,$ff,$ff,$ff,$ff
		!byte	$ff,$ff,$ff		;$15..$29 list of wanted sectors/blockindices -> $15 bytes space

;indexts = * - zeropage
		!byte	$00			;$2A
;barrier = * - zeropage
		!byte	$00			;$2B
filenum = * - zeropage
		!byte	$00			;$2C
;dest = * - zeropage
		!byte	$00,$00			;$2D ;-> 52 is always zero for free $53 will be set, sectorheader is unused during upload

in_track_ptr = * - zeropage
		!byte	0
channeladdrhi = * - zeropage
		!byte	0

;---	FlexSD disk image handler strings:
fsd_openchn_str	!byte	'#'			;Open Channel string
fsd_openchn_strl = * - fsd_openchn_str		;String length
fsd_rdblk_str	!text	"U1 "
		!byte	'0'+channelno
		!text	" 0 "			;Read block string
fsd_rdblk_tr = * - zeropage
		!text	"00 "
fsd_rdblk_sec = * - zeropage
		!text	"00"
fsd_rdblk_strl = * - fsd_rdblk_str

;cmp1		= sector
;cmp2		= index
;dst		= preamble

directory	= $0500		;directory



		;leave 8 byte of stack, enough

		* = * + 8
drivecode_stack



read_sector	;need to read sector header beforehand to compare with our wanted list, if we would seek a certain sector, the kernal routines would do
		ldx	in_track_ptr
		ldy	wanted,x		;sector on list?
		iny				;$ff + 1 = 0?
		bne	.wait_sector_data
		inx
		stx	in_track_ptr		;Check next sector
		cpx	max_sectors
		bne	read_sector		;Maybe BRA
		+break	vcpu_syscall_exit_remain	;Exit

.wait_sector_data
		ldy	track
		jsr	settracksector			;Setup track + sector numbers on command string
		;+ldzph	>fsd_rdblk_str			;(String in zeropage, ZPH value 'by design' good)
		ldy	#<fsd_rdblk_str			;"U1" string (Read block)
		ldx	#fsd_rdblk_strl			;String length
		+break	vcpu_syscall_directcommand_mem	;Read Sector
		;+ldzph	>zeropage			;Restore ZPH
		cmp	#$00				;"00, OK,00,00"?
		beq	.sectorrd_ok
		+break	vcpu_syscall_exit_remain	;If not OK, exit
.sectorrd_ok
		ldy	#$00
		ldx	in_track_ptr		;current sector number
		lda	wanted,x		;grab index from list (A with index reused later on after this call)
		dey				;blocksize full sector ($ff)
		sty	wanted,x		;clear entry in wanted list
		cmp	file_size + 1		;as we count up in $100 blocks, if index equals highbyte of size, we have loaded last block
		bne	+
		ldy	file_size		;block size of last sector (lowbyte filesize)
+
		sty	blocksize		;remember for later use
		inc	in_track_ptr		;Next sector (in track) set up
		rts				;done



send_block	lda	channeladdrhi		;Buffer address Hi
		sta	.sendloop_addr+1
		ldx	#$00			;start with 0, we will send 2 or 4 bytes of blockinfo as preamble
		lda	preamble		;$00/$FF
		and	#BLOCK_READY_FSD & BUSY_FSD
		sta	preamble		;will send 6 valid bits and signal block ready
pre_cmp = * + 1
		ldy	#$ff			;<- Preamble length, presetted
		dey				;-1 for "uindb" cycle

		;on atn (clk) going low we have alraedy first data on bus with the upcoming code

		+setready
.preloop	lda	preamble,x
+		+sendbyte			;Send BYTE
		+uindb	.preloop

		ldx	blocksize		;Pointer (backward)
		ldy	blocksize		;BYTE-no
.sendloop_addr	+ldzph	$ff			;<- Previously modified
.sendloop					;send the data block
		lda	$00,x			;Read BYTE from buffer
		+sendbyte			;Send BYTE
		+udedb	.sendloop
		+waitsend			;Wait for ATN? CLK? high
		+setbusy			;BUSY
		+ldzph	>zeropage		;Restore ZPH
		sec
		;XXX carry is set here, always, might be useful somewhen
		;XXX Y = $FF
		rts



preamble_build
		pha			;block number is in A, save

		ldy #$00

		lda firstblock
		beq +
		inc firstblock
		jsr .preamble_add_byte	;$ff as ack byte

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

		clc
		adc ld_addr+1
		jsr .preamble_add_byte	;barrier, absolute
}
		pla
		clc
		adc ld_addr+1		;add load_addr hibyte to block number to make the block number an absolute address on c64 side
		jsr .preamble_add_byte	;block_addr_hi

		ldx blocksize		;set up num of bytes to be transferred
		inx			;increase by one, as it is decreased before receiving first byte on c64
		txa

.preamble_add_byte
		sta	preamble_abs,y		;!!! Absolute address, no "STA $zp,Y" addressing mode!
		iny
		rts





turn_disc	lda	filenum
		and	#%00001111			;$F0..$FF -> $00..$0F: Disk number
!if (BITFIRE_FSDV_IMAGEOFFSET != 0) {
		sec
		sbc	#(BITFIRE_FSDV_IMAGEOFFSET & $ff)
}
		tax
		+break	vcpu_syscall_changedisk		;Change disk image
		cmp	#$00				;"00, OK,00,00"?
		beq	+
		+break	vcpu_syscall_exit_remain	;If not OK, exit
+
		;+ldzph	>fsd_openchn_str		;(String in zeropage, ZPH value 'by design' good)
		ldy	#<fsd_openchn_str
		ldx	#fsd_openchn_strl		;"#", one character
		lda	#channelno			;Channel
		+break	vcpu_syscall_open_mem
		;+ldzph	>zeropage			;Restore ZPH
		sty	channeladdrhi
		cpy	#$ff				;Error?
		bne	+
		+break	vcpu_syscall_exit_remain	;If not OK, exit
+		jsr	read_dir_sect0			;Read new disk first dir sector
		jmp	drivecode_launch

idle
!if (BITFIRE_FSDV_BITS = 2) {
		+break	vcpu_syscall_enableatnirq	;If 2 bit transmit required, enable ATN IRQ in IDLE time
}
		inc filenum		;autoinc always, so thet load_next will also load next file after a load with filenum
		dec firstblock

drivecode_launch
		;XXX TODO here it would be possible to preload next block

		+ldzph	>zeropage		;Set ZPH

.stopmotor	jsr	busyledoff

.pull_command	+setidle			;IDLE
		+urcv1				;Get BYTE from host
		+setbusy			;BUSY
		eor	#%11111111		;Flip bits

		;load file, file number is in A
;.load_file
		cmp	#BITFIRE_UPLOAD
		bne	*+5
		jmp	upload

!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 2 {
		cmp	#BITFIRE_STOPMOTOR	;STOP motor?
		beq	.stopmotor
}
		cmp	#BITFIRE_LOAD_NEXT
		beq	.load_next
		sta	filenum		;set new filenum
		bcs	turn_disc
.load_next
		;XXX TODO send out preloaded sector here if filenum = expected filenum
		jsr	busyledon

		lda	filenum		;get current or autoinced filenum
		ldx	#DIR_SECT+1
		sec
-
		sta	temp		;remember previous A
		dex
		sbc	#42
		bcs	-
+
		cpx	dirsect		;dirsect changed?
		beq	+		;nope, advance

		jsr	read_dir_sect	;fetch next dir_sect
+
		lda	temp		;fetch filenum % 42
					;file number * 6 to get proper index -> (num * 2 + num) * 2
		asl			;*2
		;clc			;can be omitted, not more than 42 files are allowed per dirsect, so carry is always cleared after asl
		adc	temp		;+1 -> * 3
		asl			;*2 -> * 6
		tay

		ldx	#$00		;reset block index
		stx	index
-					;copy over direntry to ZP
		lda	directory,y
		sta	file_descriptor,x
		iny
		inx
		cpx	#$06
		bne	-
					;a = file_size highbyte, carry set
		adc	#$00		;need to add one, as also the last partial block counts as a full block. File size is one too less, so exceptions like $xx00 in length are no problem
		sta	blocks

!if (BITFIRE_FSDV_BITS = 2) {
		+break	vcpu_syscall_disableatnirq	;If 2 bit transmit required, disable ATN IRQ
}
					;send load-address with first transferred block
.load_track
		lda	blocks
					;XXX TODO preload 1. block of next file here?
		beq	idle		;check if all blocks of file are loaded

		jsr	seek		;sets max_sectors and bitrate depending on track
--					;now find corresponding offset for first sector by subtracting until remainder (== offset) remains
!if BITFIRE_CONFIG_INTERLEAVE = 4 {
		;lax	sector		;load startsector#/offset of current rev and keep a copy in X
		 lda	sector		;load startsector#/offset of current rev and keep a copy in X
		 tax
		and	#$03
		sta	sector
} else {
		;lax	sector		;load startsector#/offset of current rev and keep a copy in X
		 lda	sector		;load startsector#/offset of current rev and keep a copy in X
		 tax
		sec			;calc modulo of .sector and keep it in .sector, XXX on interleave 4 a and #$03 would suffice, but we offer a variable interleave /o\
-
		sta	sector		;keep old value as new offset
		sbc	#BITFIRE_CONFIG_INTERLEAVE	;subtract some #sectors
		bcs	-			;until underflow
}
		;we have now the right offset to start at
-
		ldy	index			;get index
		sty	wanted,x		;write index into wantedlist
		inc	index			;advance index
		inc	blocks_on_list		;count number of blocks in list (per track num of blocks)
		dec	blocks			;all blocks loaded?
		beq	.load_wanted_blocks	;yep, end
		txa				;go for next block
		;sbx	#-BITFIRE_CONFIG_INTERLEAVE	;x += INTERLEAVE
		 sec
		 sbc	#-BITFIRE_CONFIG_INTERLEAVE	;A += INTERLEAVE
		 tax
		cpx	max_sectors		;wrap around?
		bcc	-			;nope
		lda	#BITFIRE_CONFIG_INTERLEAVE	;handle next round by increasing offset stored in .sector
		;isc	sector			;inc sector offset and compare with INTERLEAVE
		 inc	sector
		 ;sec
		 sbc	sector			;inc sector offset and compare with INTERLEAVE
		bne	--			;on zero, all done, else next round
						;XXX TODO here still blocks need to be loaded
.load_wanted_blocks				;read and transfer all blocks on wishlist
		jsr	read_sector		;returns with current blockindex in A
		jsr	preamble_build		;add blockindex and size to preamble, and handle barrier stuff there
		sty	pre_cmp			;set preamble size
		jsr	send_block		;exits with y = $ff and carry set
		dec	blocks_on_list		;last block on wishlist?
		bne	.load_wanted_blocks

;.track_finished
		iny				;Y = 0
		sty	sector			;set start pos for next track, y is always $ff after .send_block
-						;now adjust to_track and take care of skipping track 18
		lda	#18
		;sec				;set by send_block and also set if beq
		;isc	to_track
		 inc	to_track
		 sec
		 sbc	to_track
		beq	-			;skip dirtrack however
		bne	.load_track		;BRA

seek		ldy	to_track
		ldx	#$11		;max sectors on track
		cpy	#31
		bcs	+		;-> bitrate = $00
		inx
		cpy	#25
		bcs	+		;-> bitrate = $20
		inx
		cpy	#18
		bcs	+		;-> bitrate = $40
		inx			;-> bitrate = $60
		inx
+
		stx	max_sectors	;store new max sectors
		sty	track		;save target track as current track
		lda	#0
		sta	in_track_ptr		;Start new track in block 0
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
		ldy	channeladdrhi
		+tyzph
		ldx	#$00
-		lda	$00,x			;Read BYTE from buffer
		sta	directory,x		;Save to directory buffer area
		dex
		bne	-
		+ldzph	>zeropage		;Restore ZPH
		rts

;	Setup track + sector numbers on command string:
;	Y <- Track
;	X <- Sector
settracksector	sty	vcpu_bin2ascii		;Convert Track number to ASCII string
		lda	vcpu_bin2resultm
		sta	fsd_rdblk_tr+0
		lda	vcpu_bin2resultl
		sta	fsd_rdblk_tr+1		;New track set up
		stx	vcpu_bin2ascii		;Convert Sector number to ASCII
		lda	vcpu_bin2resultm
		sta	fsd_rdblk_sec+0
		lda	vcpu_bin2resultl
		sta	fsd_rdblk_sec+1		;Sector number set up
		rts

;---	LED switching:
busyledon	lda	vcpu_hid
		ora	#%00000001		;BUSY LED on
		bne	+
busyledoff	lda	vcpu_hid
		and	#%11111110		;BUSY LED off
+		sta	vcpu_hid
		rts



;		;receive end_address of code being uploaded
;		;maximum is $0110 - $04ae, buffers at $0500,$0600,$0700 can be used
;		;if original code is restored, be sure that get_byte is at the same address
upload
!if BITFIRE_FSDV_NATIVEEXTRAS = 1 {
  !serious "FSDV extras - native mode: implement me!"
} else {
		+setidle			;IDLE
		+urcv1				;Get BYTE from host (dest lo)
		+urcv1				;Get BYTE from host (dest hi)
		+urcv1				;Get BYTE from host (length lo)
		tax				;Warning: bits flipped!
		tay
		+urcv1				;Get BYTE from host (length hi: always 0, all of known codes size less than 256 BYTEs)
-		+urcv1				;Get BYTEs from host (code data, not interested)
		inx
		bne	-			;Receive all bytes from downloadable code
		+setbusy			;BUSY
		cpy	#(3-1) XOR $ff			;"JMP ($FFFC)"? Reset drive?
		bne	+
		+break	vcpu_syscall_closeall
		+break	vcpu_syscall_exit_ok
+
!src "../sectorwriter/save-drive-41.inc"
		cpy	#(drv_41_d_i_size-1) XOR $ff	;save routine init?
		bne	.nosaveinit
		jsr	busyledon
		jmp	.pull_command		;Set idle

.nosaveinit	cpy	#(drv_41_d_w_size-1) XOR $ff	;save routine sector datas?
		bne	.nosavesector
		ldx	#$ff
		ldy	channeladdrhi		;Buffer address Hi
		+setidle			;IDLE
		+urcv1				;Get BYTE from host (Track)
		eor	#%11111111
		sta	to_track
		+urcv1				;Get BYTE from host (Sector)
		eor	#%11111111
		sta	sector
		+tyzph				;Set ZPH to buffer
		ldy	#$ff
-		+urcv1				;Get BYTE from host (sector data)
		eor	#%11111111
		sta	$00,x
		+udedb	-
		+ldzph	>zeropage		;Restore ZPH
		jmp	.pull_command		;Set idle

.nosavesector	cpy	#(drv_41_d_d_size-1) XOR $ff	;save routine "DoIt!"?
		bne	.nosavedoit
		ldy	to_track
		ldx	sector
		jsr	settracksector		;Setup track + sector numbers on command string:
		;lda	#'2'
		;sta	fsd_rdblk_str+1		;"U2"
		inc	fsd_rdblk_str+1		;"U1" -> "U2"
		;+ldzph	>fsd_rdblk_str			;(String in zeropage, ZPH value 'by design' good)
		ldy	#<fsd_rdblk_str			;"U2" string (Write block)
		ldx	#fsd_rdblk_strl			;String length
		+break	vcpu_syscall_directcommand_mem	;Write Sector
		;+ldzph	>zeropage			;Restore ZPH
		cmp	#$00				;"00, OK,00,00"?
		beq	.sectorwr_ok
		cmp	#26				;Write Protect?
		beq	.sectorwr_wpon
		cmp	#72				;"72,DISK FULL,06,00"?
		beq	.sectorwr_wpon
		+break	vcpu_syscall_exit_remain	;If not OK, exit
.sectorwr_wpon
!if (BITFIRE_FSDV_BITS = 1) {
  !if (BITFIRE_SAVE_LEGACYMODE != 0) {			;Legacy mode: own status-send protocol: 1 bit mode: no status send
		lda	vcpu_hid
		ora	#%00000010		;If write protect: set DIRTY LED on
		sta	vcpu_hid
  } else {						;New mode: use BF's BYTE-send routine
		lda	#status_wpon
		+setready
		+sendbyte			;Send BYTE
		+waitsend			;Wait for CLK high
  }
} else {
  !if (BITFIRE_SAVE_LEGACYMODE != 0) {			;Legacy mode: own status-send protocol: 2 bit mode
		lda	#status_wpon
		+ucldh				;CLK Lo, DAT HiZ: READY
		+uwatl
		+uatcd	%00000100, %00001000
		+uwath
  } else {						;New mode: use BF's BYTE-send routine
		lda	#(status_wpon & BLOCK_READY_FSD)
		+sendbyte			;Send BYTE
		+waitsend			;Wait for ATN high
  }
}
.sectorwr_ok	lda	#'1'
		sta	fsd_rdblk_str+1		;Restore "U1"
		jmp	.pull_command		;Set idle

.nosavedoit	cpy	#(drv_41_d_x_size-1) XOR $ff	;save routine exit?
		bne	.nosaveexit
		;jsr	busyledoff
		;jmp	.pull_command		;Set idle
		jmp	drivecode_launch	;BUSY LED off, set idle

.nosaveexit	lda	#97
		+break	vcpu_syscall_exit_seterror
}

drivecode_end

;emit warnings only once
!if * > $0500 { !serious "Upload code is ", * - $0500, " bytes too big!" }

!ifdef drivecode_end {
	!warn $0500 - *, " bytes remaining for drivecode."
}


drivecode_size = drivecode_end - drivecode_start

;  !if (BF_DRIVE = 1541) {				;===== 1541
;drivecode_41_VIA2LED_ON	=	VIA2_LED_ON
;drivecode_41_VIA2MTR_ON	=	VIA2_MOTOR_ON
;drivecode_41_bin2ser	=	bin2ser
;drivecode_dos_bin2gcr	=	$f77f
;  } else {						;===== 1551
;drivecode_51_TCPULED_ON	=	TCPU_LED_ON
;drivecode_dos_bin2gcr	=	$f8bf
;  }
;
;drivecode_IDLE		=	IDLE
;drivecode_BUSY		=	BUSY
;drivecode_BLOCK_RDY	=	BLOCK_READY
;drivecode_to_track	=	to_track
;drivecode_sector	=	sector
;drivecode_filenum	=	filenum
;drivecode_dirsect	=	dirsect
;drivecode_head_lo	=	head_lo
;drivecode_preamb_lo	=	preamble_lo
;drivecode_preamb_hi	=	preamble_hi
;drivecode_pullcomm	=	.pull_command
;drivecode_motoron	=	motor_on
;drivecode_getbyte	=	get_byte
;drivecode_seek		=	seek
;drivecode_stopmotor	=	stopmotor
;drivecode_directory	=	directory
