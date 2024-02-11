!src "../config/music.inc"
!convtab pet
!cpu 6510

;loader zp-addresses
.barrier	= BITFIRE_ZP_ADDR + 0
.filenum	= .barrier

;depacker zp-addresses
.lz_bits	= BITFIRE_ZP_ADDR + 1
.lz_dst		= BITFIRE_ZP_ADDR + 2
.lz_end		= BITFIRE_ZP_ADDR + 4
.lz_tmp		= BITFIRE_ZP_ADDR + 6


!if BITFIRE_DEBUG = 1 {
bitfire_debug_filenum	= BITFIRE_ZP_ADDR + 7
}

!if BITFIRE_DECOMP = 0 {
bitfire_load_addr_hi = .filenum			;in case of no loadcompd, store the highbyte of loadaddress separatedly
}

		* = BITFIRE_RESIDENT_ADDR

!if BITFIRE_FRAMEWORK = 1 & BITFIRE_FRAMEWORK_FRAMECOUNTER = 1 {
link_frame_count
		!word 0
}

!if BITFIRE_NMI_GAPS = 1 & BITFIRE_DEBUG = 0 & BITFIRE_PLATFORM = BITFIRE_C64 {
!align 255,2
.lz_gap1
		nop
		nop
		nop
		nop
		nop
		nop
}


!if BITFIRE_FRAMEWORK = 1 {
!if BITFIRE_FRAMEWORK_BASEIRQ = 1 {
link_player
		pha
!if ((BF_DRIVE = 1541 and BF_PLUS4_BINCOMP>0) or BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC) {
		lda $ff13
		pha
		and #%11111101
		sta $ff13
}
		tya
		pha
		txa
		pha
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		inc $01				;should be save with $01 == $34/$35, except when music is @ >= $e000
!if BITFIRE_FRAMEWORK_MUSIC_NMI = 1 {
		lda $dd0d
} else {
		dec $d019
}
} else {
		dec $ff09
}

		jsr link_music_play

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		dec $01
}
		pla
		tax
		pla
		tay
!if ((BF_DRIVE = 1541 and BF_PLUS4_BINCOMP>0) or BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC) {
		pla
		sta $ff13
}
		pla
		rti
}

!if (BF_DRIVE = 1551 and BF_PLUS4_BINCOMP>0) {
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
	nop
	nop
	nop
}

link_music_play
!if BITFIRE_FRAMEWORK_FRAMECOUNTER = 1 {
		inc link_frame_count + 0
		bne +
		inc link_frame_count + 1
+
link_music_addr = * + 1
		jmp link_music_play_side1
}
		;this is the music play hook for all parts that they should call instead of for e.g. jsr $1003, it has a variable music location to be called
		;and advances the frame counter if needed


!if BITFIRE_DECOMP = 1 {
link_decomp	= bitfire_decomp_
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
;		;expect $01 to be $35
link_load_next_double
		;loads a splitted file, first part up to $d000 second part under IO
		jsr link_load_next_comp
link_load_next_raw_decomp
		jsr link_load_next_raw
link_decomp_under_io
		dec $01				;bank out IO
		jsr link_decomp			;depack
		inc $01				;bank in again
		rts
}
}
}


!if (BITFIRE_PLATFORM = BITFIRE_C64) {
bitfire_send_byte_
		;XXX we do not wait for the floppy to be idle, as we waste enough time with depacking or the fallthrough on load_raw to have an idle floppy
		sta .filenum			;save value
		ldx #$ff
		lda #$ef
		sec				;on first run we fall through bcc and thus end up with carry set and $0f after adc -> with eor #$30 we end up with $3f, so nothing happens on the first $dd02 write
.bit_loop
		bcc +
		adc #$1f			;on all other rounds carry is cleared here
+
		eor #$30			;flip bit 5 and toggle bite 4
		sta $dd02
		and #$1f			;clear bit
		ror <(.filenum-$ff),x		;fetch next bit from filenumber and waste cycles
		bne .bit_loop			;last bit?
						;this all could be done shorter (save on the eor #$30 and invert on floppy side), but this way we save a ldx #$ff later on, and we do not need to reset $dd02 to a sane state after transmission, leaving it at $1f is just fine. So it is worth.
						;also enough cycles are wasted after last $dd02 write, just enough for standalone, full config and ntsc \o/
		rts
} else {
	;This is defined elsewhere below for Plus/4.
}


!if BITFIRE_FRAMEWORK = 1 {
link_load_next_raw
		lda #BITFIRE_LOAD_NEXT
link_load_raw
}

bitfire_loadraw_
!if (BF_DRIVE = 1541) {
		jsr bitfire_send_byte_		;easy, open...
} else {
		jsr .bitfire_send_byte51_
}

!if BITFIRE_DECOMP = 1 {
-
		jsr .pollblock
		bcc -
;		rts				;just run into pollblock code again that will then jump to .poll_end and rts
} else {
		pha				;we are too fast in standalone mode, need to waste a few cycles for letting the drive settle
		pla
}
.pollblock
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda $dd00			;bit 6 is always set if not ready or idle/EOF so no problem with just an ASL
		asl				;focus on bit 7 and 6 and copy bit 7 to carry (set if floppy is idle/eof is reached)
		bmi .poll_end			;block ready?
} else {
  !if (BF_DRIVE = 1541) {				;===== 1541
		lda	$01
		asl				;focus on bit 7 and 6 and copy bit 7 to carry (set if floppy is idle/eof is reached)
		bpl .poll_start

!if BITFIRE_DECOMP = 0 {
		bcc .pollblock
}
		rts

  } else {						;===== 1551
		lda	$fef0			;TCBM data B76 = "mode"
		asl				;focus on bit 1 and 0 and copy bit 1 to carry (set if floppy is idle/eof is reached)
		bpl	.poll_start
		jmp	.poll_end
  }
}

.poll_start

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
  !if (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC) {
		jsr .bfsingleclock
  } else {
    !if BF_PLUS4_BINCOMP>0 and BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541DC {
    	top .get_one_byte+3
    }
  }	
}
		lda #$60			;set rts
  !if (BF_DRIVE = 1551) {				;===== 1551
		ldx	#0
  }
		jsr .bitfire_ack_		;signal that we accept data and communication direction, by basically sending 2 atn strobes by fetching a bogus byte (6 bits of payload possible, first two bist are cleared/unusable. Also sets an rts in receive loop
  !if (BF_DRIVE = 1551) {				;===== 1551
  		asl
		asl
  }
		bpl .skip_load_addr		;#$fc -> first block

  !if (BF_DRIVE = 1551) {				;===== 1551
!if (((BITFIRE_DEBUG = 1) and (BITFIRE_DECOMP = 1)) or ((BITFIRE_DEBUG = 0) and (BITFIRE_DECOMP = 0))) {
		jsr	.get_one_byte		;get dummy BYTE, on 1551: preamble size always even number of bytes
}
  }

!if BITFIRE_DEBUG = 1 {
		jsr .get_one_byte		;fetch filenum
		sta bitfire_debug_filenum
}

		jsr .get_one_byte		;fetch load/blockaddr lo
!if BITFIRE_DECOMP = 1 {			;decompressor only needs to be setup if there
		sta bitfire_lz_sector_ptr1
		sta bitfire_lz_sector_ptr2
}
		sta .bitfire_block_addr_lo	;"bitfire_load_addr_lo"	destination lowbyte

		jsr .get_one_byte		;fetch loadaddr hi, returns with a cleared carry
		sta bitfire_load_addr_hi
!if BITFIRE_DECOMP = 1 {			;decompressor only needs to be setup if there
		sta bitfire_lz_sector_ptr2 + 1
}
.skip_load_addr
!if BITFIRE_DECOMP = 1 {			;decompressor only needs to be setup if there
		jsr .get_one_byte		;fetch barrier
		sta .barrier
}

!macro plus4_clock_switch_routine ~single, ~double, ~sei_addr {
single = *
		lda $ff13
		and #%00000010		;Single clock already selected?
		eor #%00000010
		sta .bfspeed+1		;Store speed
		lda #%00011111		;Dirty Hack: Datasette RD line output and drive LOW
double = *
		sta $00				;B4 always 0 after read port
.bfspeed
		lda #0
		beq .clk_early_ok
		php
sei_addr = *
		sei				;2
		eor $ff13		;4
		sta $ff13		;4 Single Clock selected
		plp				;4 +14 clocks jitter
.clk_early_ok	
		rts
}

.bitfire_load_block
		jsr .get_one_byte		;fetch blockaddr hi
		sta .bitfire_block_addr_hi	;where to place the block?

		jsr .get_one_byte		;fetch blocklen
  !if (BF_DRIVE = 1541) {				;===== 1541
		tax
		lda #$a2			;ldx #imm
  } else {						;===== 1551
		sta	.bf51pre_evc+1
		lsr
		bcc	+			;If length is even, OK
		jsr	.get_one_byte		;If length is odd of bytes, read dummy
+
.bf51pre_evc	ldx	#$00
		lda	#$9d			;STA $nnmm,x opcode
  }


.bitfire_ack_
		sta .blockpos

!if BF_PLUS4_BINCOMP>0 {
  !if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
    !if (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC) {

		jmp .contgetbyte
		nop
		nop
		
.get_one_byte
		jmp .get_one_byte_

		+plus4_clock_switch_routine ~.bfsingleclock, ~.bfdblclock, ~bitfire_plus4_sei_1541

.contgetbyte
    }
  }
}


!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		ldy #$37
.get_one_byte
.get_one_byte_
		lda $dd00-$37,y
		sty $dd02
		lsr
		lsr
		stx .blockpos+1			;store initial x, and in further rounds do bogus writes with correct x value anyway, need to waste 4 cycles, so doesn't matter. Saves a byte (tax + stx .blockpos+1) compared to sta .blockpos+1 + nop + nop.
		ldx #$3f

		ora $dd00-$37,y			;can be omitted? 3 cycles overhead
		stx $dd02
		lsr
		lsr
		dec .blockpos+1			;waste 6 cycles and decrement

		ora $dd00-$37,y			;now ATN is 0 and ora can happen without killing bit 3
		sty $dd02
		lsr
		asr #$7e			;clear carry for free
		sta .nibble + 1
		lda #$c0

		and $dd00-$37,y			;can be omitted? 2 cycles overhead
		stx $dd02
.nibble		ora #$00			;also adc could be used, or sbc -nibble?
} else {
  !if (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC) {
		ldy	#%00001100
    !if BF_PLUS4_BINCOMP>0 {
.get_one_byte_
    } else {
.get_one_byte
.get_one_byte_
    }
		lda	$01
		sty	$01
		lsr
		lsr
		stx	.blockpos+1			;store initial x, and in further rounds do bogus writes with correct x value anyway, need to waste 4 cycles, so doesn't matter. Saves a byte (tax + stx .blockpos+1) compared to sta .blockpos+1 + nop + nop.
		ldx	#%00001000
		;nop

		eor	$01
		stx	$01
		lsr
		lsr
		dec	.blockpos+1			;waste 6 cycles and decrement
		;nop

		eor	$01
		sty	$01
		lsr
		lsr
		nop
		nop
		eor	#%00001110			;Flip back the wrong bits

		eor	$01
		stx	$01
		clc
  }
  !if (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541DC) {
		jsr .bfwait12                   ;+12 cycles
		ldy #%00001100
.get_one_byte
.get_one_byte_
		lda $01
		sty $01
		lsr
		lsr
		sta .store_recb_b1+1
		stx .blockpos+1			;store initial x, and in further rounds do bogus writes with correct x value anyway, need to waste 4 cycles, so doesn't matter. Saves a byte (tax + stx .blockpos+1) compared to sta .blockpos+1 + nop + nop.
		ldx #%00001000

		pha						;+11 cycles
		pla
!if BF_PLUS4_BINCOMP>0 {
		dop #0	
		dop #0	
} else {
		nop
		nop
}

		lda $01
		stx $01
		and #%11000000
.store_recb_b1	ora #$00
		lsr
		lsr
		sta .store_recb_b2+1
		dec .blockpos+1			;waste 6 cycles and decrement

		pha						;+7 cycles
		pla

		lda $01
		sty $01
		and #%11000000
.store_recb_b2	ora #$00
		lsr
		lsr
		sta .store_recb_b3+1

		pha						;+11 cycles
		pla
		nop
!if BF_PLUS4_BINCOMP>0 {
		dop #0	
} else {
		nop
}

		lda $01
		stx $01
		and #%11000000
.store_recb_b3	ora #$00

		jsr .bfwait12			;+12 cycles

		clc
  }
  !if (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1551) {	;===== 1551
.get_one_byte	nop
.get_one_byte_
		lda	$fef0
		stx	$fef4
		dex
		clc
  }
}

.blockpos
  !if (BF_DRIVE = 1541) {				;===== 1541
		ldx #$00
  }
.bitfire_block_addr_lo = * + 1
.bitfire_block_addr_hi = * + 2
  !if (BF_DRIVE = 1541) {				;===== 1541
bitfire_41_load_addr_lo = * + 1
  } else {						;===== 1551
bitfire_51_load_addr_lo = * + 1
  }
		sta $b00b,x
		;could also use sta ($xx),y and waste one cycle less on first lda $dd00 - $37,y
		;y should be lowbyte? or work on iny/dey?
		bne .get_one_byte_		;78 cycles per loop

		;XXX TODO to enable loading of partial start sectors: x can be set on prembale, as well as initial load_addr_lo and blocksize? but we would need to receive with incrementing x?

!if >* != >.get_one_byte { !error "getloop code crosses page!" }

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
  !if (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC) {
  		lda	#%00001111		;Datasette RD line switch to input
  		jsr .bfdblclock
  }
}
.poll_end
!if BITFIRE_DECOMP = 0 {
		;bcc .pollblock
		bcs *+5
		jmp .pollblock
}
		rts

!if BF_PLUS4_BINCOMP = 0 {
  !if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
    !if (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC) {

		+plus4_clock_switch_routine ~.bfsingleclock, ~.bfdblclock, ~bitfire_plus4_sei

    }
  }
}

!if (BF_PLUS4_BINCOMP>0 and BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541DC) {

		;nop
		;nop
		;nop
		;nop
		;nop
		;This is 5 bytes from the SC case, the swap routine 
		;can be
		;5 bytes shorter! \o/
		!byte $0f,$20,$82,$02,$60
}

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
.bfwait24       jsr .bfwait12
.bfwait12       rts				;JSR + RTS: 12 cycles

}


!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {

  !if (BF_DRIVE = 1541) {		;===== 1541

bitfire_send_byte_
		sta	.filenum		;save value
		ldx	#7
		lda	#%11001000		;ANT/CLK drive Off, DATA drive Off
.bit_loop
		lsr	.filenum
		bcs	+
		ora	#%00000001
+
		eor	#%00000011		;DATA/CLK drive changed
		sta	$01
		and	#%11111110		;Data bit clear
		jsr	.bfwait24		;24 cycles
		jsr	.bfwait12		;12 cycles
		dex
		bpl .bit_loop
		rts

  } else {						;===== 1551

.bitfire_send_byte_
		sta	$fef0				;data to TCBM data port register
		lda	#%00000000
		sta	.filenum			;Barrier = 0
		bit	$fef2
		bmi	*-3					;wait until DAV Lo
		ldx	#%11111111
		stx	$fef3				;switch TCBM data to output on plus/4 side
		sta	$fef4				;ST0=In, 1
		jsr	.bfwait12			;12 cycles
		sta	$fef3				;TCBM data DDR set to IN
		stx	$fef0			
		lda	#%00000001
		sta	$fef4				;ST0=Out, 0, cycle end in 1551 side
		jmp	.bfwait24

.bitfire_send_byte51_
		jsr .bitfire_send_byte_
		bit	$fef2
		bpl	*-3			;wait until DAV Hi
		rts
  }
}

!if  BF_DRIVE = 1551 {
  !if BF_PLUS4_BINCOMP>0 {

  !if BITFIRE_DECOMP=1 {  
	nop
  }
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
bitfire_send_byte_

	jmp .bitfire_send_byte_

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
	nop
	nop
	nop
	nop
	nop

  } else {

bitfire_send_byte_ = .bitfire_send_byte_

  }
}


!if BITFIRE_DECOMP = 1 {

;---------------------------------------------------------------------------------
; REFILL ROUTINES
;---------------------------------------------------------------------------------

.lz_lentab = * - 1
		;short offset init values
		;!byte %00000000			;2
		!byte %11011111			;0
		!byte %11111011			;1
		!byte %10000000			;3

		;long offset init values
		!byte %11101111			;offset 0
		!byte %11111101			;offset 1
		!byte %10000000			;offset 2
		!byte %11110000			;offset 3

;---------------------------------------------------------------------------------
; REFILL ROUTINES
;---------------------------------------------------------------------------------

.lz_refill_bits
bitfire_lz_sector_ptr1	= * + 1
bitfire_load_addr_hi = * + 2
		ldy $beef,x
						;store bits? happens on all calls, except when a whole literal is fetched
		bcc +				;only store lz_bits if carry is set (in all cases, except when literal is fetched for offset)
		sty .lz_bits
		rol .lz_bits
+
		inx
		bne .lz_same_page

.lz_next_page					;/!\ ATTENTION things entered here as well during depacking
		inc bitfire_lz_sector_ptr1 + 1	;use inc to keep A untouched!
		inc bitfire_lz_sector_ptr2 + 1
.lz_next_page_
.lz_skip_fetch
		php				;turned into a rts in case of standalone decomp
		pha				;preserve Z, carry, A and Y, needed depending on call
		sty .lz_tmp
.lz_fetch_sector				;entry of loop
		jsr .pollblock			;fetch another block, returns with x = 0
		bcs .lz_fetch_eof		;eof? yes, finish, only needed if files reach up to $ffxx -> barrier will be 0 then and upcoming check will always hit in -> this would suck
		lda bitfire_lz_sector_ptr1 + 1	;get current depack position
		cmp .barrier			;next pending block/barrier reached? If barrier == 0 this test will always loop on first call, no matter what .bitfire_lz_sector_ptr has as value \o/
						;on first successful .pollblock they will be set with valid values and things will be checked against correct barrier
		bcs .lz_fetch_sector		;already reached, loop
.lz_fetch_eof					;not reached, go on depacking
		ldy .lz_tmp			;restore regs + flags
		pla
		plp
.lz_same_page
		rts

!if BITFIRE_FRAMEWORK = 1 {
link_load_next_comp
		lda #BITFIRE_LOAD_NEXT
link_load_comp
}

bitfire_loadcomp_
!if (BF_DRIVE = 1541) {
		jsr bitfire_send_byte_		;returns now with x = $ff
} else {
		jsr .bitfire_send_byte51_
}
		lda #$08			;enable pollblock/fetch_sector calls (php)
		ldy #.lz_poll-.lz_skip_poll-2	;currently ldy #$0b
		;ldx #$ff			;force to load a new sector upon first read, first read is a bogus read and will be stored on lz_bits, second read is then the really needed data
		bne .loadcompd_entry		;load + decomp file


!if BITFIRE_NMI_GAPS = 1 & BITFIRE_DEBUG = 0 {
		;!ifdef .lz_gap2 {
		;	!warn .lz_gap2 - *, " bytes left until gap2"
		;}
!align 255,2
.lz_gap2
!if .lz_gap2 - .lz_gap1 > $0100 {
		!error "code on first page too big, second gap does not fit!"
}
		nop
		nop
		nop
}

bitfire_decomp_
.lz_end_of_file	= * + 1				;point to rts, this is always reachable \o/
		lda #$60			;disable calls
		ldy #.lz_skip_end-.lz_skip_poll-2	;#$17
		ldx #$00			;start with first byte of block
.loadcompd_entry
		sta .lz_skip_fetch
		sty .lz_skip_poll + 1
		;address stuff is already set by loadraw_/pollblock

;---------------------------------------------------------------------------------
; DECRUNCHER
;---------------------------------------------------------------------------------

.lz_decrunch
-
		jsr .lz_refill_bits		;fetch depack addr
		sty .lz_dst-1,x			;x = 0, x = 1 and x = 2
!if BITFIRE_DECOMP_ZERO_OVERLAP = 0 {
		cpx #$02
} else {
		cpx #$04
}
		bne -
		;sec				;set for free by last compare
.lz_type_refill
		jsr .lz_refill_bits		;refill bit buffer .lz_bits

		;******** Start the next match/literal run ********
.lz_type_check
		bcc .lz_do_match
		beq .lz_type_refill		;we will fall through on entry

		;******** Process literal run ********

		lda #$00
-
		rol				;-> a = $01 after first round
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits		;kills y
		bcc .lz_lrun_gotten

		asl .lz_bits
		bne -
		jsr .lz_refill_bits
		bne -

.lz_lrun_gotten
		sta .lz_lcopy_len		;Store LSB of run-length
		ldy #$00
.lz_lcopy
bitfire_lz_sector_ptr2	= * + 1			;Copy the literal data, forward or overlap is getting a pain in the ass.
		lda $beef,x
		sta (.lz_dst),y
		inx
		bne +
		jsr .lz_next_page
+
		iny
.lz_lcopy_len = * + 1
		cpy #$00
		bne .lz_lcopy

		tya
		beq .lz_maximum			;maximum literal run, bump sector pointers and so on and force new type bit
						;XXX TODO can we reuse the same code? In one case continue with match, in other case redecide
		clc
		adc .lz_dst
		sta .lz_dst
		bcc .lz_do_match
		inc .lz_dst+1
						;no need for a type bit, after each literal a match follows, except for maximum runlength literals

		;******** Process match ********

.lz_do_match
		lda #$01			;this could be made shorter by using the last bitfetch of the upcoming loop and restoring the carry again by a cmp #$02. Saves bytes, but makes things slower, as eof check is also done with all short matches then

		asl .lz_bits			;first length bit (where a one identifies
		bne *+5				;a two-byte match)
		jsr .lz_refill_bits
		bcc .lz_get_offs		;all done, length is 2, skip further bitfetches (and eof check)
-
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		rol

		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		bcc -
.lz_got_len
		tay				;XXX TODO could this be placed elsewhere to make the tay obsolete?
		beq .lz_end_of_file		;A 257-byte (=>$00) run serves as a sentinel, but not with zero-overlap, except when depacking from a non inplace address, then it is still appended
.lz_get_offs
		sta .lz_mcopy_len		;store length at final destination

		lda #%11000000			;fetch 2 more prefix bits
		rol				;previous bit is still in carry \o/
-
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		rol
		bcs -

		beq .lz_8_and_more		;0 + 8 bits to fetch, branch out before table lookup to save a few cycles and one byte in the table, also save complexity on the bitfetcher
		tay
		lda .lz_lentab,y
-						;same as above
		asl .lz_bits			;XXX same code as above, so annoying :-(
		bne *+5
		jsr .lz_refill_bits
		rol
		bcs -

		bmi .lz_less_than_8		;either 3,4,6 or 7 bits fetched -> highbyte will be $ff
.lz_8_and_more
		jsr .lz_refill_bits
		eor #$ff			;5 of 13, 2 of 10, 0 of 8 bits fetched as highbyte, lowbyte still to be fetched
		sta .lz_tmp			;XXX this is a pain in the arse that A and Y need to be swapped :-(
		tya
		ldy .lz_tmp
		top
.lz_less_than_8
		ldy #$ff			;XXX TODO silly, y is set twice in short case
		adc .lz_dst			;subtract offset from lz_dst
		sta .lz_m+1
		tya				;hibyte
		adc .lz_dst+1
		sta .lz_m+2

		ldy #$ff			;The copy loop. This needs to be run
						;forwards since RLE-style matches can overlap the destination
.lz_mcopy
		iny
.lz_m		lda $face,y			;copy one byte
		sta (.lz_dst),y
.lz_mcopy_len	= * + 1
		cpy #$ff
		bne .lz_mcopy

		tya				;advance destination pointer
;		sec				;XXX TODO carry set = type check needed, cleared (literal) = match follows anyway
		adc .lz_dst
		sta .lz_dst

!if BITFIRE_DECOMP_ZERO_OVERLAP = 0 {
.lz_skip_poll	bcc +
.lz_maximum	inc .lz_dst+1			;this is also used by maximum length
		bcs .lz_skip_end
+

} else {
		bcc +				;proceed to check
.lz_maximum
		inc .lz_dst+1			;advance hi byte
;		lda .lz_dst			;if entering via .lz_maximum, a = 0, so we would pass the following check only if the endadress is @ $xx00
+						;if so, the endaddress can't be $xx00 and the highbyte check will fail, as we just successfully wrote a literal with type bit, so the end address must be greater then the current lz_dst, as either another literal or match must follow. Can you still follow me?! :-D
		eor .lz_end			;check end address
.lz_skip_poll	bne .lz_poll			;all okay, poll for a new block

		eor .lz_dst+1			;check highbyte
		eor .lz_end+1
		bne .lz_skip_end		;skip poll, so that only one branch needs to be manipulated
		;sta .barrier			;clear barrier and force to load until EOF, XXX does not work, but will at least force one additional block before leaving as barrier will be set again upon next block being fetched. Will overlap be > than 2 blocks? most likely not? CRAP, tony taught me that there is /o\
		lda #$ff
		sta bitfire_load_addr_hi	;needed if the barrier method will not work out, plain jump to poll loop will fail on stand alone depack?
		jmp .lz_next_page_		;load any remaining literal blob if there, or exit with rts in case of plain decomp (rts there instead of php). So we are forced until either the sector_ptr reaches $00xx or EOF happens, so nothing can go wrong
						;XXX TODO could be beq .lz_next_page_ but we get into trouble with 2nd nmi gap then :-(
}

.lz_poll

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		;XXX TODO can be omitted as done in pollblock, but a tad faster this way
		bit $dd00
		bvs .lz_skip_end
} else {
  !if ((BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC) or (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541DC)) {	;=====	1541
		bit	$01
		bvs .lz_skip_end
  }
  !if (BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1551) {	;=====	1551
    !if BF_PLUS4_BINCOMP = 0 {
		bit	$fef0
		bvs .lz_skip_end
    } else {
		jmp *+4
		nop
    }
  }
}

		stx .lz_tmp			;save x, lz_tmp is available at that moment
!if BF_DRIVE = 1551 and BF_PLUS4_BINCOMP>0 {
		jsr .pollblock
} else {
		jsr .poll_start			;yes, fetch another block
}
		ldx .lz_tmp			;restore x
.lz_skip_end
						;literals needing an explicit type bit
		asl .lz_bits			;fetch next type bit
		jmp .lz_type_check
						;XXX TODO refill_bits -> do no shifting yet, but do in code, so we could reuse the asl ?!
}	;endif DECOMP

!if BITFIRE_AUTODETECT = 1 {
link_chip_types
link_sid_type			;%00000001	;bit set = new, bit cleared = old
  !if (BITFIRE_PLATFORM = BITFIRE_C64) {
link_cia1_type			;%00000010
link_cia2_type			;%00000100
  } else {
link_drive_type
  }

  !if BF_DRIVE = 1541 {
		!byte $00
  } else {
		!byte BITFIRE_DRIVE_1551
  }
}

!if BF_PLUS4_BINCOMP>0 {
  !if BF_DRIVE = 1541 {
bitfire_41_poll_start	=	.poll_start
  } else {
bitfire_51_poll_start	=	.poll_start
  }
bitfire_swaprecv_addr
} else {
  !ifdef BITFIRE_SAVE_ADDR {
bitfire_saveroutine_addr = BITFIRE_SAVE_ADDR
  } else {
    !if ((* & $007f) = 0) {
bitfire_saveroutine_addr
    } else {
.base = (* & $ff80) + $0080
* = .base
bitfire_saveroutine_addr
    }
  }
}

;  !if BF_PLUS4_BINCOMP = 0 {
bitfire_resident_size = * - BITFIRE_RESIDENT_ADDR
;}

!if BF_DRIVE = 1551 {
bitfire_51_tcbmbase_addr = .bitfire_send_byte_ + 1
}

; Export SD2IEC - FlexSD/VCPU patch addresses:
fsdvp_getonebyte_addr = .get_one_byte_
fsdvp_blockpos_addr = .blockpos
fsdvp_block_addr = .bitfire_block_addr_lo-1

; Single BYTE recv. entry point for SAVE routine
!if BF_DRIVE = 1541 {
bitfire_41_bitfire_ack	=	.bitfire_ack_
}
