		.macro link_load_next_raw_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_load_next_raw
		.endmacro

		.macro link_load_next_comp_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_load_next_comp
		.endmacro

		.macro link_decomp_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_decomp
		.endmacro

		.macro link_load_next_double_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_load_next_double
		.endmacro

		.macro link_load_next_raw_decomp_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_load_next_raw_decomp
		.endmacro

		.macro wait_frame_count cnt
                .local l
l
			lda link_frame_count+0
			cmp #<cnt
			lda link_frame_count+1
			sbc #>cnt
			bcc l
		.endmacro
