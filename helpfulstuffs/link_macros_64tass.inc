
link_load_next_raw_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_load_next_raw
		.endm

link_load_next_comp_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_load_next_comp
		.endm

link_decomp_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_decomp
		.endm

link_load_next_double_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_load_next_double
		.endm

link_load_next_raw_decomp_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_load_next_raw_decomp
		.endm

wait_frame_count .macro
l2		lda link_frame_count+0
		cmp #<(\1)
		lda link_frame_count+1
		sbc #>(\1)
		bcc l2
		.endm
