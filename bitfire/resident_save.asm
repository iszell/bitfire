!src "drivecode_acme_1541.inc"
!src "drivecode_acme_1551.inc"

!if (BITFIRE_PLATFORM=64) {
  !src "loader_acme_c64.inc"
} else {
  !if (BITFIRE_PLUS4_MODE=11) {
    !src "loader_acme_plus4_multi.inc"
  } else {
    !src "loader_acme_plus4_1551.inc"
  }
}

!src "sector_routs.asm"

	* = BITFIRE_SAVE_ADDR

	+sector_routines 0, BF_DRIVE