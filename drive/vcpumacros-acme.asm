;------------------------------------------------------------------------------
;---	SD2IEC VCPU macros
;---	"acme" version
;---	  https://sourceforge.net/projects/acme-crossass/
;---	V1.1.2
;------------------------------------------------------------------------------
;---	VCPU interrupt codes:
vcpu_error_align	=	$80
vcpu_error_address	=	$40
vcpu_error_illegalfc	=	$20
vcpu_error_hangup	=	$10
vcpu_error_card		=	$08
vcpu_error_atn		=	$04
vcpu_error_rwaddr	=	$02
vcpu_functioncall	=	$01

;---	VCPU SYSCALL codes:
vcpu_syscall_exit_ok		=	$00
vcpu_syscall_exit_seterror	=	$01
vcpu_syscall_exit_fillederror	=	$02
vcpu_syscall_exit_remain	=	$03
vcpu_syscall_disableatnirq	=	$11
vcpu_syscall_enableatnirq	=	$12
vcpu_syscall_setfatparams	=	$13
vcpu_syscall_directcommand	=	$21
vcpu_syscall_directcommand_mem	=	$22
vcpu_syscall_open		=	$23
vcpu_syscall_open_mem		=	$24
vcpu_syscall_close		=	$25
vcpu_syscall_closeall		=	$26
vcpu_syscall_refillbuffer	=	$27
vcpu_syscall_getchannelparams	=	$28
vcpu_syscall_changedisk		=	$31

vcpu_paramoffset	=	96

vcpu_commandbuffer	=	$fc00
vcpu_errorbuffer	=	$fd00
vcpu_param_position	=	vcpu_errorbuffer + vcpu_paramoffset + 0
vcpu_param_lastused	=	vcpu_errorbuffer + vcpu_paramoffset + 1
vcpu_param_eoi		=	vcpu_errorbuffer + vcpu_paramoffset + 2

;---	I/O addresses:
vcpu_iobase		=	$fe00
vcpu_version		=	vcpu_iobase + $00
vcpu_memiosize		=	vcpu_iobase + $01
vcpu_hid		=	vcpu_iobase + $02
vcpu_unitno		=	vcpu_iobase + $03
vcpu_diag		=	vcpu_iobase + $04
vcpu_b2dconverter	=	vcpu_iobase + $07
vcpu_bin2decimal	=	vcpu_b2dconverter + 0
vcpu_bin2ascii		=	vcpu_b2dconverter + 1
vcpu_bin2result		=	vcpu_b2dconverter + 0
vcpu_bin2resultl	=	vcpu_bin2result + 0
vcpu_bin2resultm	=	vcpu_bin2result + 1
vcpu_bin2resulth	=	vcpu_bin2result + 2
vcpu_atnsrqout		=	vcpu_iobase + $0a
vcpu_atnsrqin		=	vcpu_iobase + $0b
vcpu_clkdatout		=	vcpu_iobase + $0c
vcpu_clkdatin		=	vcpu_iobase + $0d
vcpu_clkio		=	vcpu_iobase + $0e
vcpu_datio		=	vcpu_iobase + $0f

;------------------------------------------------------------------------------
;===	"SysCall":
!macro break .fcode {
    !ifdef .fcode {
      !if ((.fcode > 255) | (.fcode < 0)) {
        !error "Parameter must be 0..255 range!"
      }
    }
		!by	$00, .fcode
}

;===	SPH/ZPH:
!macro tyzph {
		!byte	$44
}
!macro tzphy {
		!byte	$54
}
!macro ldzph .addrhi {
    !ifdef .addrhi {
      !if ((.addrhi > 255) | (.addrhi < 0)) {
        !error "Parameter must be 0..255 range!"
      }
    }
		!byte	$02, .addrhi
}
!macro tysph {
		!byte	$d4
}
!macro tsphy {
		!byte	$f4
}
!macro ldsph .addrhi {
    !ifdef .addrhi {
      !if ((.addrhi > 255) | (.addrhi < 0)) {
        !error "Parameter must be 0..255 range!"
      }
    }
		!byte	$22, .addrhi
}

;	Binary to decimal ASCII chars:
!macro btasc {
    !error "`BTASC` command deprecated. Use the converter peripheral instead."
}

;===	µOp codes:
;	UWDTL: Wait for DAT Low:
!macro uwdtl {
		!byte	$03
}
;	UWDTH: Wait for DAT High:
!macro uwdth {
		!byte	$13
}
;	UWCKL: Wait for CLK Low:
!macro uwckl {
		!byte	$23
}
;	UWCKH: Wait for CLK High:
!macro uwckh {
		!byte	$33
}
;	UWATL: Wait for ATN Low:
!macro uwatl {
		!byte	$43
}
;	UWATH: Wait for ATN High:
!macro uwath {
		!byte	$53
}

;	USDTL: Set DAT to Low:
!macro usdtl {
		!byte	$83
}
;	USDTH: Set DAT to HighZ:
!macro usdth {
		!byte	$93
}
;	USCKL: Set CLK to Low:
!macro usckl {
		!byte	$a3
}
;	USCKH: Set CLK to HighZ:
!macro usckh {
		!byte	$b3
}
;	USATL: Set ATN to Low:
!macro usatl {
		!byte	$c3
}
;	USATH: Set ATN to HighZ:
!macro usath {
		!byte	$d3
}

;	UCLDL: Set CLK to Low / DAT to Low:
!macro ucldl {
		!byte	$0b
}
;	UCLDH: Set CLK to Low / DAT to HighZ:
!macro ucldh {
		!byte	$1b
}
;	UCHDL: Set CLK to HighZ / DAT Low:
!macro uchdl {
		!byte	$2b
}
;	UCHDH: Set CLK to HighZ / DAT to HighZ:
!macro uchdh {
		!byte	$3b
}

;	UATDT: A register bit to DAT line:
!macro uatdt .bitmask {
    !ifdef .bitmask {
      !if ((.bitmask > 255) | (.bitmask < 0)) {
        !error "Parameter must be 0..255 range!"
      }
    }
		!byte	$4b, .bitmask
}
;	UDTTA: DAT line to A register bit:
!macro udtta .bitmask {
    !ifdef .bitmask {
      !if ((.bitmask > 255) | (.bitmask < 0)) {
        !error "Parameter must be 0..255 range!"
      }
    }
		!byte	$5b, .bitmask
}
;	UATCK: A register bit to CLK line:
!macro uatck .bitmask {
    !ifdef .bitmask {
      !if ((.bitmask > 255) | (.bitmask < 0)) {
        !error "Parameter must be 0..255 range!"
      }
    }
		!byte	$6b, .bitmask
}
;	UCKTA: CLK line to A register bit:
!macro uckta .bitmask {
    !ifdef .bitmask {
      !if ((.bitmask > 255) | (.bitmask < 0)) {
        !error "Parameter must be 0..255 range!"
      }
    }
		!byte	$7b, .bitmask
}
;	UATCD: A register bits to CLK/DAT lines:
!macro uatcd .clkbitmask, .datbitmask {
    !if ((.clkbitmask > 255) | (.clkbitmask < 0) | (.datbitmask > 255) | (.datbitmask < 0)) {
      !error "Parameters must be 0..255 range!"
    }
		!byte	$8b, .clkbitmask, .datbitmask
}
;	UCDTA: CLK/DAT lines to A register bits:
!macro ucdta .clkbitmask, .datbitmask {
    !if ((.clkbitmask > 255) | (.clkbitmask < 0) | (.datbitmask > 255) | (.datbitmask < 0)) {
      !error "Parameters must be 0..255 range!"
    }
		!byte	$9b, .clkbitmask, .datbitmask
}

;	ULBIT: Serial Lines "BIT" test:
!macro ulbit {
		!byte	$db
}

;	UINDB: Increment X, Decrement Y, Branch if Y no underrun
!macro uindb .address {
    !ifdef .address {
      !if (((.address - * - 2) > 127) | ((.address - * - 2) < -128)) {
        !error "Address too far!"
      }
    }
		!byte	$ab, (.address - * - 2)
}
;	UDEDB: Decrement X, Decrement Y, Branch if Y no underrun
!macro udedb .address {
    !ifdef .address {
      !if (((.address - * - 2) > 127) | ((.address - * - 2) < -128)) {
        !error "Address too far!"
      }
    }
		!byte	$bb, (.address - * - 2)
}

;	USND1: Send A register bits to DAT
!macro usnd1 {
		!byte	$63
}
;	URCV1: Receive bits from DAT to A register
!macro urcv1 {
		!byte	$73
}
;	USND2: Send A register bitpairs to DAT/CLK
!macro usnd2 {
		!byte	$e3
}
;	URCV2: Receive bitpairs from DAT/CLK to A register
!macro urcv2 {
		!byte	$f3
}

;	UDELY: Delay "xT":
!macro udely .time {
    !ifdef .time {
      !if ((.time > 255) | (.time < 0)) {
        !error "Parameter must be 0..255 range!"
      }
    }
		!byte	$fb, .time
}
;	UDLY1: Delay "1T":
!macro udly1 {
		!byte	$ea
}

;	UTEST:
!macro utest {
    !error "`UTEST` command deprecated."
}
;------------------------------------------------------------------------------
