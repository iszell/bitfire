!convtab pet
!cpu 6510

  !if (BF_DRIVE = 1541) {				;===== 1541
    !src "drivecode-1541.tlb"
  } else {
    !src "drivecode-1551.tlb"
  }

_btstrp_addr	= $0700

	* = _btstrp_addr

.bootstrap_start

  !if (BF_DRIVE = 1541) {				;===== 1541
		lda #$12
		sta $0a
		lda #DIR_SECT
		sta $0b

		;lda #$c0			;BUMP command
		;sta $02
		;lda $02
		;bmi *-2

-		lda	#$b0			;SEEK command
		sta	$02
		lda	$02
		bmi	*-2
		cmp	#1			;SEEK OK?
		bne	-			;If not, retry

		;fetch first dir sect and by that position head at track 18 to have a relyable start point for stepping
-		lda #$80
		sta $02
		lda $02
		bmi *-2
		cmp	#1			;Sector read OK?
		bne	-			;If not, retry

		;motor and LED is on after that

		sei

		ldx #<drivecode_stack-1
		txs
;		lda #$00
;-
;		sta $0100,x
;		dex
;		bpl -

		;lda #$08
		;sta $1800

		lda #%01111010		;DDR set bits for drivenumber to 0, ATN out, CLK out and DATA out are outputs
		sta $1802

		lda #%11101110		;CB2 manual output high (read), CB1 low, CA2 manual output high (byte ready), CA1 low (NC)
		sta $1c0c

		lda #%00000001		;PB disable latching, PA enable latching (content for $1c01 is then latched)
		sta $1c0b

		lda #$7f		;disable all interrupts
		sta $180e
		sta $1c0e

		sta $180d		;clear all IRQ flags to ack possibly pending IRQs
		sta $1c0d

		;cli			;now it is save to allow interrupts again, as they won't happen anymore

		;lda	#%00000100	;CLK in
		lda	#%00000001	;DAT in
		bit	$1800
		beq	*-3		;Wait for line low

		ldy #$00

		;ldx #BUSY		;DAT to low, signal that we are ready for transfer
		ldx	#BLOCK_READY	;CLK to Low, signal that we are ready for transfer
		stx $1800		;  Pull line to Low

		;;wait for atn coming high
		;bit $1800
		;bpl *-3
		bit	$1800
		bne	*-3		;Wait for line high

		sty $1800		;clear all lines and set bit 7 as bit counter, to allow data in and clk in to be set/cleared by host
		ldx $1800
.get_block
		lda #$80
-
		cpx $1800		;did a bit arrive? (bit flip in data in, atn is dropped in same go in first bit)
		beq -
		ldx $1800		;load register
		;;;bmi .done		;(Modified: ATN not used for this task anymore...)
		cpx #$04		;push bit into carry
		ror			;shift in
		bcc -			;do until our counter bit ends up in carry
.block = * + 1
		sta drivecode_start,y

		iny
		bne .bytecnt
		inc .block+1

.bytecnt	cpy	#<drivecode_end
		bne	.get_block
		lda	.block+1
		cmp	#>drivecode_end
		bne	.get_block
;.done

		ldx #BUSY
		stx $1800

		;;wait for atn coming low
		;bit $1800
		;bmi *-3
  } else {                        			;===== 1551 Bootstrap routine
		lda	#$ff
		sta	$96		;LED always ON / IT
		lda	#$12
		sta	$0c
		lda	#DIR_SECT
		sta	$0d

		;lda	#$c0			;BUMP command
		;sta	$04
		;lda	$04
		;bmi	*-2

-		lda	#$b0			;SEEK command
		sta	$04
		lda	$04
		bmi	*-2
		cmp	#1			;SEEK OK?
		bne	-			;If not, retry

		;fetch first dir sect and by that position head at track 18 to have a relyable start point for stepping
-		lda	#$80
		sta	$04
		lda	$04
		bmi	*-2
		cmp	#1			;Sector read OK?
		bne	-			;If not, retry

		;motor and LED is on after that

		sei

		ldx	#<drivecode_stack-1
		txs
;		lda	#$00
;-
;		sta	$0100,x
;		dex
;		bpl	-

		lda	#%01101111
		sta	$00			;CPU PORT DDR: set default value

		lda	#%00011100		;ST1/ST0: Input, others: default
		sta	$4005
		lda	#%00000000
		sta	$4003			;TIA PORTA DDR: set TCBM DATA to Input
		sta	$4004			;TIA PORTB DDR: Read datas from disk
		sta	$4006			;TIA Control Register = $00: Normal Operation
		lda	$4002
		and	#%00010100		;ST1/ST0 lines: %00, ACK=0: Ready data accept from plus/4
		ora	#%00010000		;Read Mode, DEV line not changed, ST=%00, ACK=0
		sta	$4002

		;cli		;NEVER ENABLE interrupt at the future, the SEI command is
				; only one way to disable interrupt in 1551

		ldy	#$02			;On START: SKIP CPU port
		ldx	#$ff			;"End" sign
.get_block	bit	$4002           	;DAV check
		bmi	.get_block		;Wait LOW
		lda	$4000			;Read TCBM DATA
.blockw		sta	drivecode_start,y
-		bit	$4002			;DAV check
		bpl	-
		cpx	$4000			;TCBM DATA = $FF? download ready?
		beq	.done
		iny
		bne	.get_block
		inc	.blockw+2
		bne	.get_block	;BRA

.done		lda	$4002
		ora	#%00001000		;ACK=1
		sta	$4002
  }

		lda #$12
		sta track
		sta to_track
		jsr seek
		jmp drivecode_launch

.bootstrap_end
.bootstrap_size = .bootstrap_end - .bootstrap_start
