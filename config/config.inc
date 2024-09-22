;Platform selection
BITFIRE_C64   = 64
BITFIRE_PLUS4 = 16

!ifndef BITFIRE_PLATFORM {
	BITFIRE_PLATFORM = BITFIRE_PLUS4
}

;The type of drive/receiver on Plus/4 
BITFIRE_PLUS4_1541SC = 1			;Drive: 1541, Single Clock
BITFIRE_PLUS4_1541DC = 2			;Drive: 1541, Double Clock
BITFIRE_PLUS4_1551   = 8			;Drive: 1551, Double Clock

!if (BITFIRE_PLATFORM = BITFIRE_C64) {
BITFIRE_SID = $d400
} else {
BITFIRE_SID = $fd40
BITFIRE_NAE = $fd80
}

;options for resident part
BITFIRE_RESIDENT_ADDR           = $0200
BITFIRE_ZP_ADDR                 = $04

;options for sector write
;BITFIRE_SAVE_ADDR		= $1c00		;If defined, save routines compiled this area. If not defined, address calculated automatically
BITFIRE_SAVE_LEGACYMODE		= 0		;If 1, save routines use own protocol to send status (use for compatibility mode, not needed to new project)

;Framework options, certain features can be switched on/off separatedly
BITFIRE_FRAMEWORK               = 1     ;Enable/Disable whole framework
BITFIRE_FRAMEWORK_BASEIRQ       = 1     ;Include a base irq handler into the framework
BITFIRE_FRAMEWORK_FRAMECOUNTER  = 1     ;Include framecounter handling
BITFIRE_FRAMEWORK_MUSIC_NMI     = 0     ;should base irq run in NMI or raster IRQ?

;Include a decompressor and by that on the fly decrunching?
BITFIRE_DECOMP                  = 1     ;Include decompressor and on the fly decompression
BITFIRE_DECOMP_ZERO_OVERLAP     = 1     ;Packed files have no overlap and can be completely depacked in place. Bitnax by default creates files that meet that spec. If you wish the old behaviour, use --overlap

BITFIRE_NMI_GAPS                = 0     ;Leaves gaps @ $0200 and $0300 (if resident part is at default location) for NMI handlers, needed for stable NMIs with THCM's digistuff.

BITFIRE_AUTODETECT              = 1     ;Autodetect CIA + SID and store the results in the resident part for later use, so upcoming parts do not need an own detection anymore but can read out that values again

BITFIRE_WORLD_DOMINATION        = 1     ;Soon to come :-D

;options for installer
BITFIRE_INSTALLER_C64_ADDR      = $0a00
BITFIRE_INSTALLER_PLUS4_ADDR    = $1200
;BITFIRE_RESIDENT_AUTOINST       = 1     ;Set this to zero if you want to do the install of the resident part on your own, but why would you want to do so anyway? :-D
BITFIRE_1551_ONLY8		= 0	;If set, (plus/4 only) 1551 drive use at only unit #8 (Do not use, only for compatibility)
BITFIRE_UNITSCAN_PRN            = 1     ;If set, unit scanner check 4..7 units
BITFIRE_DRIVESELECTOR		= 1	;Drive selector code: 1: "Complex Drive Selector"

;options for drivecode
BITFIRE_CONFIG_MOTOR_ALWAYS_ON  = 0     ;If you want to be annoying, let the motor spin during the whole demo :-D (0 = Normal mode, 1 = Always On, 2 = Always On, but stoppable)
BITFIRE_CONFIG_INTERLEAVE       = 4     ;Turned out to be the best choice, if you really think you know it better, change it and get slowed down :-)
BITFIRE_FSDV_BITS               = 1	;SD2IEC - FlexSD/VCPU byte-receiver mode: 1 or 2 bits (2 bits used ATN, only one device allowed on the bus)
BITFIRE_FSDV_NATIVEEXTRAS	= 0	;SD2IEC - FlexSD/VCPU extra functions native: 0: Legacy (like as 1541), 1: native (unimplemented yet)
BITFIRE_FSDV_40TRK		= 0	;SD2IEC - FlexSD/VCPU 40 track image: 0: Normal mode, 1: required for 40 track image support in firmware, VCPU-R2, check VCPU revision
BITFIRE_FSDV_IMAGEOFFSET	= 0	;SD2IEC - FlexSD/VCPU disk image offset: if it is not 0, this number subtracted from the requested disk number (If you don't know what you could use it for, leave it at zero)

;add extra debug functionality, not intended for demos
BITFIRE_DEBUG                   = 0

;constants, better don't touch
BITFIRE_UPLOAD                  = $ed   ;Original: $80. This version: the "file names" is $00..$EC range, max. 237 BF files on disk
BITFIRE_STOPMOTOR               = $ee   ;In "MOTOR_ALWAYS_ON" mode "2", this code STOP it
BITFIRE_LOAD_NEXT               = $ef
BITFIRE_REQ_DISK                = $f0
BITFIRE_IS_INCLUDED             = 1

BITFIRE_SID_NEW                 = 1
BITFIRE_CIA_NEW                 = 2
BITFIRE_DRIVE_SD2IEC            = 16
BITFIRE_NAE_CARD                = 32
BITFIRE_NO_SID                  = 64
BITFIRE_DRIVE_1551              = 128
