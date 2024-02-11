		ifnconst link_dasm_inc
		link_dasm_inc = 1
		mac link_load_next_raw_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_raw
		endm

		mac link_load_next_comp_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_comp
		endm

		mac link_decomp_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_decomp
		endm

		mac link_load_next_double_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_double
		endm

		mac link_load_next_raw_decomp_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_raw_decomp
		endm

		mac wait_frame_count
.sau
			lda link_frame_count+0
			cmp #<{1}
			lda link_frame_count+1
			sbc #>{1}
			bcc .sau
		endm

		endif
