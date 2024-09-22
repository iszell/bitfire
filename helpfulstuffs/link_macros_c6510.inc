;if def link_c6510_inc == 0
;const link_c6510_inc = 1

		include "word.s"
		include "irq.s"

		macro m_link_load_next_raw_jmp n:16
		{
			lda #(n-1)>>8
			pha
			lda #(n-1)&255
			pha
			jmp link_load_next_raw
		}

		macro m_link_load_next_comp_jmp n:16
		{
			lda #(n-1)>>8
			pha
			lda #(n-1)&255
			pha
			jmp link_load_next_comp
		}

		macro m_link_decomp_jmp n:16
		{
			lda #(n-1)>>8
			pha
			lda #(n-1)&255
			pha
			jmp link_decomp
		}

		macro m_link_load_next_double_jmp n:16
		{
			lda #(n-1)>>8
			pha
			lda #(n-1)&255
			pha
			jmp link_load_next_double
		}

                macro m_link_load_next_raw_decomp_jmp n:16
		{
                        lda #(n-1)>>8
                        pha
                        lda #(n-1)&256
                        pha
                        jmp link_load_next_raw_decomp
                }

		macro m_wait_frame_count cnt:16
		{
.w:
			lda link_frame_count+0
			cmp #(cnt & 255)
			lda link_frame_count+1
			sbc #(cnt >> 8)
			bcc .w
		}

;endif
