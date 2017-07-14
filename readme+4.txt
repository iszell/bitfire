Bitfire+4
^^^^^^^^^

Credits:
 - Original C64 loader: Bitbreaker/Oxyron
 - Plus/4 port: Bubis/Resource, BSZ/NST, Krill/Plush (we borrowed some of your ideas ;))
 - Testing: Luca/FIRE, Chronos/ACE, Csaba/New Wave

GitHub:
 - http://github/dotscha/bitfire

NAE:
 - http://bsz.amigaspirit.hu/nae/


What is this?
^^^^^^^^^^^^^

This is a Plus/4 port of Bitfire. Bitfire is a modern IRQ loader for C64 and
the 1541 with its own lz packer, BAM compatible file format and image writer
tool, that has a really small memory footprint. Please read readme.txt for
further details.

Differences from the C64 version
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First of all, the Bitfire+4 source code can be compiled to both platforms, this is driven
by the BITFIRE_PLATFORM constant in bitfire/config.inc. If you set BITFIRE_C64 there you
can compile original C64 version of the loader (it is set to BITFIRE_PLUS4 by default).
A few things just doesn't make sense on Plus/4, like the NMI gap, link_load_next_double, 
link_decomp_under_io, CIA detection. You won't find those in the Plus/4 version.
The SID base address is not obvious on Plus/4, you have to change BITFIRE_SID if that is
not $FD40 for you. ($FD40 is recommended of course.) The type of the SID chip or missing 
SID is detected correctly by the installer and NST's Audio Extension (NAE) is also 
detected.
Bitnax has a new option --plus4 that will create a Plus/4 executable when used with
the --sfx <addr> option.
The biggest difference is that Bitfire+4 comes with multiple transfer routines. You can
read more about the two 2bit receivers for the 1541 and the 1551 protocol below.

Protocols
^^^^^^^^^

1541 2bit ATN double clock receiver
-----------------------------------

This one uses the original C64/1541 protocol, the receiver code does not switch between
single clock and double clock and does not disable interrupts. The code is optimized for
double clock and about as fast as the C64 protocol when running on the border. The 
obvious advantage of this receiver is that you don't have to worry about the timing of
your interrupts. The disadvantage is that transfer is slow when running on the screen.

1541 2bit ATN single clock receiver
-----------------------------------

This one also uses the original protocol, the receiver code switches to single clock mode
when transfering data from the drive. It also disables interrupts for a few cycles by
default when flipping the clock bit in $ff13. This is only neccessary if you modify $ff13
in your interrupt routines and can be disabled like this:

	bit link_drive_type          ;$3ff
	bmi *+7                      ;skip if 1551
	lda #$ea                     ;nop
	sta bitfire_plus4_sei_1541

The advantage of this receiver is equal speed on screen and border. This is ideal if you
have a loader picture in a simple hardware gfx mode on the screen and you want maximum
speed. The disadvantage of it is obviously the trouble you may have with interrupts
because of single clock mode and interrupt blocking mentioned earlier (in case you have
to keep that sei). To make sure your code (eg. your sid/ted music player) doesn't slow
down, you have to do something like this:

	lda $ff13
	pha             ;store current state
	and #$fd        ;clear the single clock bit
	sta $ff13
	
	...
	
	pla             ;restore previous state
	sta $ff13

The default interrupt handler starting at link_player ($202) has this double clock
protection feature by default in 1541 mode.

1541 default receiver and swap routine
--------------------------------------

The single clock routine is the 1541 default in the binary release, but a swap routine
is also provided, so you can switch to the double clock routine and back any time you
want.

The swap routine starts at bitfire_plus4_swap_receiver ($400), it is not part of the
loader, so you can overwrite it any time as soon as you don't need it.

The 1551 loader has a single "rts" at this address for compatibility reasons, so if
you swap receivers for the 1541 case, the same code will work just fine in the 1551 
case too.

1551 8bit protocol
------------------

This protocol transfers a byte in 20 cpu cycles during block transfer. The 1551 loader
is able to load a full track in two rounds. 1551 as unit 9 is not supported yet.

Note: We have a working 19.5 cycles version too, but it's a bit overcomplicated.
Even 18 cycles are possible if you have a faster TIA replacement like BSZ in his
hacked 1551 (used to be 1541) drive. 

1541/1551 or "multi" installer
------------------------------

This installer has the resident and drive code for both drives, and it tries to use
a 1551 drive if available. The residen parts are made binary compatible for this
installer, so almost all entry points and addresses are the same in the include file
for both drives (loader_*_plus4_multi.inc). The labels that only make sense for one 
of the drives hava a _1541 or _1551 prefix. These are:

 - bitfire_plus4_sei_1541: the address of "sei" in 1541 single clock mode, see above.
 - bitfire_load_addr_lo_1541: the address of the latest block being transferred
 - bitfire_load_addr_lo_1551: the same in the 1551 case

The last two is not really usefull, but you may use them for visualizing the loading
process if you want.
The MSB (bit 7) of link_drive_type ($3ff) is one if Bitfire is using a 1551 drive, so
it is easy to handle the 1541/1551 cases like this

  bit link_drive_type
  bmi .case1551
  ...

.case1551
  ...

Binary Release
^^^^^^^^^^^^^^

The purpose of the binary release is to provide you with the compiled version of
Bitfire+4. You don't have to have Linux/Cygwin, etc. installed in order to start using
the loader, but you can always download the source from GitHub and compile it for
yourself with different settings. The binary release contains all features of the
loader and it is still small, so you probably won't miss anything if you use it. ;)

The structure of the latest binary release:
 - acme/
   - acme.exe: ACME cross-assembler version 0.96.2
 - bitfire/
   - bitnax.exe: packer based on doynax.
   - d64write.exe: image writer tool that can copy files to disk image in normal
       and Bitfire format.
   - installer_plus4_multi.prg: Installer for 1541/1551 with drive detection.
   - loader_*_plus4_multi.inc: include files contating all important routine and memory addresses
       of the 1541/1551 loader.
   - installer_plus4_1551.prg: Inetaller for 1551 only.
   - loader_*_plus4_1551.inc: include file for the 1551 loader.
   - link_macros_*.inc: useful macros for many popular cross-assemblers
   - reset_drive.asm: reset drive routine for 1541/1551
   - request_disc.asm: request (next) disc routine for 1541/1551
 - example/
   - main.asm: simple example demonstrating loader installation, loading raw files and 
       loading and depacking compressed files.
   - build.bat: simple build script demostrating how to compress files for Bitfire,
       how to create disc and copy files to it in normal and bitfire format.
   - bitmap*.prg: five bitmaps

Bitfire+4 2017.07.12:
 - Resident part: $200-$3ff
 - Zero page usage: $04-$09
 - 1541 2bit ATM double/single clock receiver routines
 - 1551 8bit protocol
 - Optional 1541 receiver swap routine: $400-$471
 - Precompiled installer for 1551 and 1541/1551 multi installer
 - load raw, load+decomp, decomp routines
 - Request disc and reset drive routines working with both 1541/1551 drives
 - Basic irq handler for music, cpu clock protection, frame counter
 - SID chip detection, missing SID detection and NAE detection 
 - Include files and useful macros for the most popular cross-assemblers
 - Win32 packer and image writer tool, ACME cross-assembler
 - Simple code example and Win32 build script that uses the included assembler
 
Bitfire+4 2017.05.05:
 - Resident part: $200-$3ff
 - Zero page usage: $04-$09
 - 2bit ATN double clock receiver
 - load raw, load+decomp, decomp routines
 - Basic irq handler for music, frame counter, partial SID detection
 - Include files and useful macros for the most popular cross-assemblers
 - Win32 packer and image writer tool, ACME cross-assembler
 - Precompiled installer for Plus/4 
 - Simple code example and Win32 build script that uses the included assembler
 
Bitfire+4 2017.05.21:
 - Resident part: $200-$3ff
 - Zero page usage: $04-$0a
 - 2bit ATN double clock receiver
 - 2bit ATN single clock receiver
 - Receiver swap routine: $400-$471 (included with both installers, please see above)
 - load raw, load+decomp, decomp routines
 - Basic irq handler for music with double clock protection
 - Frame counter, partial SID detection
 - Include files and useful macros for the most popular cross-assemblers
 - Win32 packer and image writer tool, ACME cross-assembler
 - Precompiled installers for Plus/4 (bitfire/installer_plus4_41*c_swap.prg)
 - Simple code example and Win32 build script that uses the included assembler
 
