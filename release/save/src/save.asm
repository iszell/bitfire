!if BF_DRIVE = 1541 {
  !src "drivecode_acme_1541.inc"
} else {
  !src "drivecode_acme_1551.inc"
}

!if (BITFIRE_PLATFORM=64) {
  !src "../../bitfire/loader_acme_c64.inc"
} else {
  !if (BITFIRE_PLUS4_MODE=11) {
    !src "../../bitfire/loader_acme_plus4_multi.inc"
  } else {
    !src "../../bitfire/loader_acme_plus4_1551.inc"
  }
}

!src "sector_routines.asm"

	* = BITFIRE_SAVE_ADDR

	+sector_routines 0, BF_DRIVE