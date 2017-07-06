.detect_sid
		sei
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda #$00
		sta $d015
}
		lda #$ff
!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		cmp $d012
} else {
		cmp $ff1d
}
		bne *-3

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
		lda	$ff13
		pha
		ora	#%00000010			;Single clock selected
		sta	$ff13

		lda	#$e0
		sta	BITFIRE_NAE+$d			;Select NAE mode
		lda	BITFIRE_NAE+$f			;Read NAE Rev.Code
		and	#%11100000
		cmp	#%11100000			;Synergy SID-card present?
		beq	+				; If yes, config shouldn't be attempted
		lda	#$d0				;886 KHz, $D4xx not captured
		sta	BITFIRE_NAE+$d
		lda	#$f1				;SID access on $FE80..$FE9F is enabled
		sta	BITFIRE_NAE+$d
		lda	#$de				;DigiBlaster D/A enabled
		sta	BITFIRE_NAE+$d
		lda	BITFIRE_NAE+$e			;Read config state
		and	#%00001111			;Mask no-SID feature bits
		cmp	#%00001100			;Config is success?
		bne	+
		lda	#BITFIRE_NAE_CARD
		ora	link_chip_types
		sta	link_chip_types			;NAE card noted.
+
		jsr	.detect_sid_clr			;SID reg. clear
		lda	#$f0
		sta	BITFIRE_SID+$14			;Sustain level: $F
		lda	#$01
		sta	BITFIRE_SID+$12			;Gate bit 1: start sound (but no waveform selected, silent...)
		ldx	#$ff
-		jsr	.detect_wframe			;Wait a frame
		cpx	BITFIRE_SID+$1c
		bne	.sid_missing
		txa
		sec
		sbc	#$11
		tax
		bcc	.sid_present
		and	#$f0
		sta	BITFIRE_SID+$14			;Next sustain level
		jmp	-

.sid_missing	jsr	.detect_sid_clr			;SID reg. clear
		lda	#BITFIRE_NO_SID
		ora	link_chip_types
		sta	link_chip_types			;SID missing noted.
		jmp	.detect_end

.sid_present	jsr	.detect_sid_clr			;SID reg. clear
		lda	#$ff
		sta	BITFIRE_SID+$0e
		sta	BITFIRE_SID+$0f			;MAX freq
		lda	#%00110000			;Triangle+Sawtooth selected
		sta	BITFIRE_SID+$12
		ldy	#$20
		ldx	#$00
-		lda	BITFIRE_SID+$1b			;Osc.3 output READ
		bmi	+
		dex
		bne	-
		dey
		bne	-
		beq	++
+		lda	#BITFIRE_SID_NEW
		ora	link_chip_types
		sta	link_chip_types			;SID missing noted.
++		jsr	.detect_sid_clr			;SID reg. clear
}

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
		lda #$ff
		sta BITFIRE_SID+$12
		sta BITFIRE_SID+$0e
		sta BITFIRE_SID+$0f
		lda #$20
		sta BITFIRE_SID+$12
		lda BITFIRE_SID+$1b
		eor #$01
		and #$01
		ora link_chip_types	;0 = old, 1 = new sid
		sta link_chip_types
}

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
.detect_end	pla
		sta	$ff13				;Clock restored
		jmp	.detect_over

.detect_sid_clr	lda	#0
		ldx	#0
-		sta	BITFIRE_SID,x
		inx
		cpx	#$19
		bne	-				;SID registers clear
		rts

.detect_wframe	pha
		lda	#$fe
		cmp	$ff1d
		bne	*-3
		lda	#$ff
		cmp	$ff1d
		bne	*-3
		pla
		rts
}


!if (BITFIRE_PLATFORM = BITFIRE_C64) {
.detect_cia
		;lda $d011
		;bmi *-3
		;lda $d011
		;bpl *-3

		sei
		lda #$35
		sta $01
		lda #<.detect_2
		sta $fffa
		lda #>.detect_2
		sta $fffb

		lda $dd0d
		lda #$81
		sta $dd0d

		lda #$04
		sta $dd04
		lda #$00
		sta $dd05

		sta $02

		lda #%10011001
		sta $dd0e

		lda $dd0d
		lda $dd0d
		inc $02
		jmp *

.detect_2
		lda $dd0d
		pla
		pla
		pla
		lda $02
		asl
		eor #$02
		ora link_chip_types
		sta link_chip_types

		lda #$37
		sta $01
}

!if (BITFIRE_PLATFORM = BITFIRE_PLUS4) {
.detect_over
;		ldx	#0
;		lda	#$30
;-		lsr
;		asl	link_chip_types
;		rol
;		sta	$0c00,x
;		inx
;		cpx	#8
;		bne	-
;-		inc	$0c08
;		jmp	-
}
