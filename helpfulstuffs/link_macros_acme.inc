		;load next file as a raw file (do not decomp on the fly) and jump to .addr
		!macro link_load_next_raw_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_load_next_raw
		}

		;load next file and decomp on the fly, then jump to .addr
		!macro link_load_next_comp_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_load_next_comp
		}

		;decomp previously loaded file and jump to .addr
		!macro link_decomp_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_decomp
		}

		;load next file and decompress on the fly, then load next file raw, disable IO and decompress it, then enable IO again -> a full file that was split into two parts is loaded, with a portion going under IO
		!macro link_load_next_double_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_load_next_double
		}

		;load file raw and decomp
		!macro link_load_next_raw_decomp_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_load_next_raw_decomp
		}

		!macro clear_frame_count {
			lda #$00
			sta link_frame_count + 0
			sta link_frame_count + 1
		}

		!macro wait_frame_count .cnt {
-
			lda link_frame_count+0
			cmp #<.cnt
			lda link_frame_count+1
			sbc #>.cnt
			bcc -
		}

		!macro set_depack_pointers .addr {
			lda #<.addr
			sta bitfire_lz_sector_ptr1
			sta bitfire_lz_sector_ptr2
			lda #>.addr
			sta bitfire_lz_sector_ptr1 + 1
			sta bitfire_lz_sector_ptr2 + 1
		}
