#ifndef link_dream_inc
		link_dream_inc = 1

		#macro link_load_next_raw_jmp(addr)
		{
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_load_next_raw
		}

		#macro link_load_next_comp_jmp(addr)
		{
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_load_next_comp
		}

		#macro link_decomp_jmp(addr)
		{
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_decomp
		}

		#macro link_load_next_double_jmp(addr)
		{
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_load_next_double
		}

		#macro link_load_next_raw_decomp_jmp(addr) {
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_load_next_raw_decomp
                }

		#macro clear_frame_count
		{
			lda #$00
			sta link_frame_count + 0
			sta link_frame_count + 1
		}

		#macro wait_frame_count(cnt)
		{
.(
sau:
			lda link_frame_count+0
			cmp #<{cnt}
			lda link_frame_count+1
			sbc #>{cnt}
			bcc sau
.)
		}

#endif
