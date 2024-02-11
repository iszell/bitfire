;	SD2IEC - FlexSD/VCPU 1 bit patch
;
; 1-bit data transfer is more convenient for SD2IEC drives. (More than one
; drives allowed on the serial bus.) This patch replaces the data receiving
; routine in the computer-side program ('resident code') with the 1-bit
; version. (The data-send was originally 1 bit, so that routine was not
; affected.) The SD2IEC drive is faster by design, which is why 1-bit
; data transfer is also adequate in most situations.

!convtab pet
!cpu 6510

!src "../config/config.inc"

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
  !src "resident-c64-41.incp"
	* = fsdvp_getonebyte_addr

.getbyte	bit	$dd00		;Drive READY acked?
		bvs	.acked		;If CLK high, get data
		lda	#%00111111	;DAT to HiZ
		sta	$dd02
		lda	#%00011111	;DAT to Low, ACK
		sta	$dd02
		bne	.acked
		nop

.acked		ldy	#%01111111

.getbytecyc	dex
		stx	.blockpos+1
		lda	#%01111111	;B7=0, last shift clear carry
.bytecyc	ldx	#%00101111	;CLK Low
		cpy	$dd00
		stx	$dd02
		ror
		ldx	#%00111111	;CLK HiZ
		cpy	$dd00
		stx	$dd02
		ror
		bcs	.bytecyc
} else {
  !src "resident-c264-41db.incp"
	* = fsdvp_getonebyte_addr

.getbyte	bit	$01		;Drive READY acked?
		bvs	.acked		;If CLK high, get data
		lda	#%11001000
		sta	$01		;DAT to HiZ
		lda	#%11001001
		sta	$01		;DAT to Low, ACK
		jmp	.acked
		nop
		nop
		nop
		nop

.acked		ldy	#%11001000	;CLK HiZ

.getbytecyc	dex
		stx	.blockpos+1
		ldx	#%01101010	;CLK Low, plus B7/B6/B5 trim to compare
		cpx	$01
		stx	$01
		ror
		cpx	$01
		sty	$01
		ror
		cpx	$01
		stx	$01
		ror
		cpx	$01
		sty	$01
		ror
		cpx	$01
		stx	$01
		ror
		cpx	$01
		sty	$01
		ror
		cpx	$01
		stx	$01
		ror
		cpx	$01
		sty	$01
		ror
		clc
}
.blockpos	ldx	#$ff		;<- patched
.block		sta	$b00b,x
		bne	.getbytecyc	;plus/4 = 84 cycles per loop (Original 2 bit DC transfer: 127); C64 = 127 cycles per loop (Original 2 bit transfer: 78)

!if (.getbyte != fsdvp_getonebyte_addr) {!serious "Patch address error: 'get_one_byte'! ", .getbyte, " / ", fsdvp_getonebyte_addr}
!if (.blockpos != fsdvp_blockpos_addr) {!serious "Patch address error: 'blockpos'! ", .blockpos, " / ", fsdvp_blockpos_addr}
!if (.block != fsdvp_block_addr) {!serious "Patch address error: 'block'! ", .block, " / ", fsdvp_block_addr}
