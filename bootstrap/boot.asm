!src "../bitfire/loader_acme.inc"
!src "../link_macros_acme.inc"
!src "../music.inc"

		* = $0100
!if SIDE = 1 {
		;load music
		jsr link_load_next_comp

		lda #$00
		;reset frame counter
		sta link_frame_count + 0
		sta link_frame_count + 1
		tax
		tay
		jsr link_music_init_side1

		;add if music entry changes on current side
		;lda #<link_music_play
		;sta link_music_addr + 1

		sei

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda #$35
		sta $01
} else {
		sta $ff3f
}
		;vsync
		;bit $d011
		;bpl *-3
		;bit $d011
		;bmi *-3

		ldx #<link_player
		lda #>link_player
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
!if BITFIRE_FRAMEWORK_MUSIC_NMI = 1 {
		stx $fffa
		sta $fffb
		lda #$81
		sta $dd0d
		lda $dd0d
		lda #$c7
		sta $dd04
		lda #$4c
		sta $dd05

		lda #$ff
		cmp $d012
		bne *-3

		;enable timer irq
		lda #$11
		sta $dd0e

		;better cease all other IRQs
		lda #$7f
		sta $dc0d
		lda $dc0d

		lda #$00
		sta $d019
		sta $d01a

                dec $d019
} else {
		stx $fffe
		sta $ffff
		lda #$7f
		sta $dc0c
		sta $dd0c
		lda $dc0c
		lda $dd0d
		lda #$01
		sta $d019
		sta $d01a
}
} else {
		stx $fffe
		sta $ffff

		lda #2			;raster irq
		sta $ff0a
		lda #$cc
		sta $ff0b
		lda $ff09
		sta $ff09
}

        cli

		;load first part
                jsr link_load_next_raw
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		dec $01
}
		jsr link_decomp
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		inc $01
}
		;keep demo in sync. $20 frames for loading + $20 extra frames to compensate for jitter in loading times
		+wait_frame_count $40
		;lda link_frame_count+0
		;cmp #<.cnt
		;lda link_frame_count+1
		;sbc #>.cnt
		;bcc -

		;start part at the same point in time
		jmp link_exit
}


!if SIDE = 2 {
}
