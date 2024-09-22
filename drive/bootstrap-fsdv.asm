;	SD2IEC - FlexSD/VCPU bootstrap

BF_DRIVE	=	101	;FlexSD

!convtab pet
!cpu 6510

    !src "vcpumacros-acme.asm"
    !src "drivecode-fsdv.tlb"

_btstrp_addr	= directory	;$0500		;Temporary dir buffer

	* = _btstrp_addr

.bootstrap_start
.imageno	!byte	0				;<- First byte of bootstrap; modified by installer: requested image no

.bootstrap_go	+uchdh					;CLK+DAT HiZ

		lda	vcpu_hid
		ora	#%00000001			;BUSY LED On
		sta	vcpu_hid
		sei

		+ldsph	>drivecode_stack-1
		ldx	#<drivecode_stack-1
		txs

		ldy	#0
		;+uwckl					;Wait for CLK Lo
		;+uchdl					;Set CLK HiZ + DAT Lo, sign to start VCPU code
		;+uwckh					;Wait for CLK Hi
		;+uchdh					;Set CLK + DAT to HiZ
		+uwdtl					;Wait for DAT Lo
		+ucldh					;Set DAT HiZ + CLK Lo, sign to start VCPU code
		+uwdth					;Wait for DAT Hi
		+uchdh					;Set CLK + DAT to HiZ

.get_data	+uwdtl					;Wait for DAT Lo
		+uckta	%00000001			;Move CLK to B0
		+uwdth					;Wait for DAT Hi
		+uckta	%00000010			;Move CLK to B1
		+uwdtl					;Wait for DAT Lo
		+uckta	%00000100			;Move CLK to B2
		+uwdth					;Wait for DAT Hi
		+uckta	%00001000			;Move CLK to B3
		+uwdtl					;Wait for DAT Lo
		+uckta	%00010000			;Move CLK to B4
		+uwdth					;Wait for DAT Hi
		+uckta	%00100000			;Move CLK to B5
		+uwdtl					;Wait for DAT Lo
		+uckta	%01000000			;Move CLK to B6
		+uwdth					;Wait for DAT Hi
		+uckta	%10000000			;Move CLK to B7
		eor	#%11111111			;Flip bits

.wrtaddr	sta	drivecode_start,y
		iny
		bne	+
		inc	.wrtaddr+2
+		cpy	#<drivecode_end
		bne	.get_data
		lda	.wrtaddr+2
		cmp	#>drivecode_end
		bne	.get_data

		+uchdl					;CLK HiZ, DAT Lo: BUSY

		ldx	.imageno			;Selected disk-image
		bmi	+				;If negative, image set is not required
		+break	vcpu_syscall_changedisk		;Set
		cmp	#$00				;OK?
		beq	+
		+break	vcpu_syscall_exit_remain	;If not OK, exit

+		+ldzph	>.boot_openchn_str
		ldy	#<.boot_openchn_str
		ldx	#.boot_openchn_strl		;"#", one character
		lda	#channelno			;Channel
		+break	vcpu_syscall_open_mem
		+ldzph	>_drvcode_addr			;Restore ZPH
		sty	channeladdrhi
		cpy	#$ff				;Error?
		bne	+
		+break	vcpu_syscall_exit_remain	;If not OK, exit
+
		;lda	#$12
		;sta	track
		;sta	to_track			;Sets not required, default values determined by downloaded data
		;lda	#$ff
		;sta	dirsect				;Not required, drivecode forces ReRead actual dir sector
		;jsr	seek

		jmp	drivecode_launch

.boot_openchn_str
		!byte	'#'
.boot_openchn_strl = * - .boot_openchn_str

.bootstrap_end
.bootstrap_size = .bootstrap_end - .bootstrap_start

!if .bootstrap_size > 112 { !serious "FSDV bootstrap code is ", .bootstrap_size, " bytes too big!" }

bootstrap_fs_start	=	.bootstrap_start
bootstrap_fs_end	=	.bootstrap_end
bootstrap_fs_size	=	.bootstrap_size
bootstrap_fs_run	=	.bootstrap_go
