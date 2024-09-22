!convtab pet
!cpu 6510

!if BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC {
!src "resident-c264-41sb.inc"
} else {
!src "resident-c264-41db.inc"
}

	* = bitfire_swaprecv_addr

!if BF_DRIVE = 1551 {
;  !if BF_PLUS4_BINCOMP = 1 {
swapper_plus4_swap_receiver:
  rts
;  }
} else {

  !if BF_PLUS4_BINCOMP = 2 {
swapper_plus4_swap_receiver:

    !if BITFIRE_DECOMP = 1 {
.dest = BITFIRE_RESIDENT_ADDR + 110 	;this is based on binary comparison results, don't touch
.swap_data_len = 195-110+1-5
    } else {
.dest = BITFIRE_RESIDENT_ADDR + 94 	;this is based on binary comparison results, don't touch
.swap_data_len = 184-94+1-5
    }

	ldx #.swap_data_len
-
	lda .dest-1,x
	pha
	lda .swap_data-1,x
	sta .dest-1,x
	pla
	sta .swap_data-1,x
	dex
	bne -
	
	lda bitfire_41_poll_start
	eor # $20 xor $0c	;jsr vs. nop
	sta bitfire_41_poll_start
	rts
	
.swap_data:

    !if BITFIRE_PLUS4_MODE = BITFIRE_PLUS4_1541SC {
!bin "resident-c264-41db", .swap_data_len, .dest-BITFIRE_RESIDENT_ADDR
    } else {
!bin "resident-c264-41sb", .swap_data_len, .dest-BITFIRE_RESIDENT_ADDR
    }
  }
}

!if BF_DRIVE = 1541 {
swapper_swap41_size = * - bitfire_swaprecv_addr
} else {
swapper_swap51_size = * - bitfire_swaprecv_addr
}

!ifdef BITFIRE_SAVE_ADDR {
swapper_saveroutine_addr = BITFIRE_SAVE_ADDR
} else {
  !if ((* & $007f) = 0) {
swapper_saveroutine_addr
  } else {
.base = (* & $ff80) + $0080
* = .base
swapper_saveroutine_addr
  }
}
