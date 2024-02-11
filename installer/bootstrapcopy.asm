;	Copy bootstrap code to the different drives:

;	Install bootstrap to CBM drive:
;	Y:X <- Bootstrap (datas)
!if ((BF_DRV_1541 + BF_DRV_1551) != 0) {
install_bootstrap_cbm
;	Copy parameters:
		stx	z_ptrl
		sty	z_ptrh
		ldy	#0
.bootstrdc	lda	(z_ptr),y	;Set source/ dest address
		sta	z_src,y
		iny
		cpy	#4
		bne	.bootstrdc
		lda	(z_ptr),y	;Bootstrap length (×32 BYTEs)
		pha

;	Initialize drive:
		lda	#0
		sta	z_st
		lda	#$6f
		jsr	opentolisten	;Open command channel
		lda	#'i'
		jsr	iecout
		jsr	unlisten

;	Send bootstrap code to drive mem:
		pla
		tax			;Bootstrap length (×32 BYTEs)
.bs_loop	lda	#$6f
		jsr	opentolisten	;Open command channel
		lda	#'m'
		jsr	iecout
		lda	#'-'
		jsr	iecout
		lda	#'w'
		jsr	iecout
		lda	z_destl		;Target address
		jsr	iecout
		lda	z_desth
		jsr	iecout
		lda	#$20		;Payload
		jsr	iecout
		ldy	#0
-		lda	(z_src),y
		jsr	iecout
		iny
		cpy	#$20
		bne	-

		tya
		clc
		adc	z_srcl
		sta	z_srcl
		bcc	*+4
		inc	z_srch

		tya
		clc
		adc	z_destl
		sta	z_destl
		bcc	*+4
		inc	z_desth

		jsr	unlisten
		dex
		bne	.bs_loop

;	Execute bootstrap in drive mem:
		lda	#$6f
		jsr	opentolisten	;Open command channel
		lda	#'m'
		jsr	iecout
		lda	#'-'
		jsr	iecout
		lda	#'e'
		jsr	iecout
		ldy	#5
		lda	(z_ptr),y	;Start address Lo
		jsr	iecout
		iny
		lda	(z_ptr),y	;Start address Hi
		jsr	iecout
		jsr	unlisten
		sei
		rts
}

;	Install bootstrap to SD2IEC + VCPU:
;	Y:X <- Bootstrap (datas)
!if (BF_DRV_SD2IEC = 1) {
install_bootstrap_vcpu
;	Copy parameters:
		stx	z_ptrl
		sty	z_ptrh
		ldy	#0
.bootstrdc2	lda	(z_ptr),y	;Set source/ dest address
		sta	z_src,y
		iny
		cpy	#4
		bne	.bootstrdc2
		lda	(z_ptr),y	;Bootstrap length
		sta	.sd2ibslen+1

		lda	#$6f
		jsr	opentolisten	;Open command channel
		lda	#'z'
		jsr	iecout
		lda	#'w'
		jsr	iecout
		lda	z_destl		;Target address
		jsr	iecout
		lda	z_desth
		jsr	iecout
		ldy	#0
-		lda	(z_src),y
		jsr	iecout
		iny
.sd2ibslen	cpy	#$ff		;<- Self-modified
		bne	-
		jsr	unlisten

;	Execute downloaded code with VCPU:
		lda	#$6f
		jsr	opentolisten	;Open command channel
		lda	#'z'
		jsr	iecout
		lda	#'e'
		jsr	iecout
		ldy	#5
		lda	(z_ptr),y	;Start address Lo
		jsr	iecout
		iny
		lda	(z_ptr),y	;Start address Hi
		jsr	iecout
		jsr	unlisten
		sei
		rts
}
