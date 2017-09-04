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
Bitfire+4 also contains a save routine for both platforms.

Differences from the C64 version
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First of all, the Bitfire+4 source code can be compiled to both platforms, this is driven
by the BITFIRE_PLATFORM constant in bitfire/config.inc. If you set BITFIRE_C64 there you
can compile the original C64 version of the loader (it is set to BITFIRE_PLUS4 by default). 
The Bitfire+4 binary releas contants the C64 verion of the loader too.
A few things just doesn't make sense on Plus/4, like the NMI gap, link_load_next_double, 
link_decomp_under_io, CIA detection. You won't find those in the Plus/4 version of the
loader. The SID base address is not obvious on Plus/4, you have to change BITFIRE_SID if
that is not $FD40 for you. ($FD40 is recommended of course.) The type of the SID chip or 
missing SID is detected correctly by the installer and NST's Audio Extension (NAE) is also 
detected.
Bitnax has a new option --plus4 that will create a Plus/4 executable when used with
the --sfx <addr> option.
The biggest difference between the C64 and the Plus/4 loader is that the Plus/4 loader 
comes with multiple transfer routines. You can read more about the two 2bit receivers 
for the 1541 and the 1551 protocol below.

How to build from source
^^^^^^^^^^^^^^^^^^^^^^^^

We recommend to use the precompiled version, but if you want to compile for yourself go 
into the bitfire folder and use "make". This will create an installer and include files
based on config.inc (see readme.txt)

"make installers" will build Bitfire for all supported platforms and drives and it will
create an installer_*.prg and a bunch of loader_*.inc files for each.

"make save_routines" will compile the save routine (save_*.prg) and create common include
files (save_*.inc) for all supported platforms and drives. See more details below.

Plus/4 protocols and receivers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1541 2bit ATN double clock receiver
-----------------------------------

This one uses the original C64/1541 protocol, the receiver code does not switch between
single clock and double clock and does not disable interrupts. The code is optimized for
double clock and about as fast as the C64 protocol when running on the border. The 
obvious advantage of this receiver is that you don't have to worry about the timing of
your interrupts and you don't have to mess with $ff13 (see below). The disadvantage is 
that transfer is slower when running on the screen.

1541 2bit ATN single clock receiver
-----------------------------------

This one also uses the original protocol, the receiver code switches to single clock mode
when transfering data from the drive. It also disables interrupts for a few cycles by
default when flipping the clock bit in $ff13. 
The advantage of this receiver is equal speed on screen and border. This is ideal if you
have a loader picture in a simple hardware gfx mode on the screen and you want maximum
speed. The disadvantage of it is obviously the trouble you may have with interrupts
because of single clock mode and interrupt blocking mentioned earlier. To make sure your
code (eg. your sid/ted music player and other code you run in irq) doesn't slow down, you 
have to do something like this:

	lda $ff13
	pha             ;store current state of $ff13
	and #$fd        ;and clear the single clock bit
	sta $ff13       ;to run in double clock mode
	
	...
	
	pla             ;restore previous state
	sta $ff13

The default interrupt handler starting at link_player ($202) has this double clock
protection feature by default in 1541 mode.

A more advanced way to restore the clock bit looks something like this:

    ...
    
    pla             ;this is the stored value of $ff13
    and #2          ;but only the clock bit
    ora $ff13       ;needs to be
    sta $ff13       ;restored

The above code only restores the clock bit (assuming that is remains zero after it was
cleared) and allows the change of the other bits of $ff13.

And some more advanced stuff: If you don't modify $ff13 in your irq routines, you can 
disable an unnecessary SEI in the single clock receiver that can delay your intererupts
for no good reason:

	bit link_drive_type          ;$3ff
	bmi *+7                      ;skip if 1551
	lda #$ea                     ;nop
	sta bitfire_plus4_sei_1541

Make sure you only do this with the single clock routine, you can brick the loader 
otherwise. :)


1541 default receiver and swap routine
--------------------------------------

The single clock routine is the 1541 default in the binary release, but a swap routine
is also provided, so you can switch to the double clock routine and back any time you
want.

The swap routine starts at bitfire_plus4_swap_receiver ($400), it is not part of the
loader, so you can overwrite it any time as soon as you don't need it.

The 1551 loader has a single RTS at this address for compatibility reasons, so if
you swap receivers for the 1541 case, the same code will work just fine in the 1551 
case too.

1551 8bit protocol
------------------

This protocol transfers a byte in 20 cpu cycles during block transfer. The 1551 loader
is able to load a full track in two rounds. 1551 as unit 9 is not supported yet.

Note: We have a working 19.5 cycles version too, but it's a bit overcomplicated.
Even 18 cycles is possible if you have a faster TIA replacement like BSZ in his
hacked 1551 (used to be 1541) drive. 

1541/1551 or "multi" installer
------------------------------

This installer has the resident and drive code for both drives, and it tries to use
a 1551 drive if available. The residen parts are made binary compatible for this
installer, so almost all entry points and addresses are the same in the include file
for both drives (loader_*_plus4_multi.inc). The labels that only make sense for one 
of the drives hava a _1541 or _1551 prefix. These are:

 - bitfire_plus4_sei_1541: the address of SEI in 1541 single clock mode, see above.
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

Hardware detection
^^^^^^^^^^^^^^^^^^

The installer detects the drive type, and popular Plus/4 hardware extensions. The 
detection results are stored in link_drive_type, link_chip_types, link_sid_types
all pointing to $03ff. The meaning of its bits are:

bit 7: 1551 drive detected
bit 6: no SID chip detected (TED only)
bit 5: NAE card detected
bit 4-1: unused
bit 0: new SID (8580) detected


Save routine
^^^^^^^^^^^^

This was added for the request of game developers. It's purpose is only to save a
few blocks on disk. It cannot create new normal or Bitfire files, it can only 
overwrite one or a series of tracks advancing t/s by BITFIRE_CONFIG_INTERLEAVE.
The typical usage would be saving a highscore file in Bitfire format, taking note
of the starting t/s and using the save routine to update the file on disk.

There are common include files for all platforms and drives, you only have to 
set BITFIRE_SAVE_ADDR to the load address of the save routine. The binary release
contains precompiled save routines for all installers, they start at:

 - save_c64.prg              : $0400
 - save_plus4_1551.prg       : $0400
 - save_plus4_multi_1541.prg : $0480
 - save_plus4_multi_1551.prg : $0480

If this doesn't suit you just go into save/src, modify build.bat and build your
own version.

Please note that we have two routines for the Plus/4 multi loader. Let's see this
case as an example:

Let's suppose your program already intalled the Plus/4 multi loader, earlier you 
saved a four blocks long file in Bitfire format as #0 (starting a t/s=1/0), and you 
also saved the 1541 and 1551 save routines in Bitfire format as file #1 and #2. 
Now, you want to overwrite file #0 with data from $2000 to $23ff. This is what you
do:

    ;The save routines start at $0480, so set that address first
    ;and include "save_acme.inc"

    BITFIRE_SAVE_ADDR = $0480
    !src "../save/save_acme.inc"

    ;Load the right save routine depending on the drive type.
    lda #1               ; 1541
    bit link_drive_type
    bpl *+4              ; unless
    lda #2               ; 1551
    jsr bitfire_loadraw_

    ;Init the save routine. This will upload code to the drive memory and start
    ;the drive's motor.
    jsr bitfire_save_init

    ;Set up what you want to save
    lda #$00
    sta bitfire_save_data_ptr
    lda #$20
    sta bitfire_save_data_ptr + 1

    ;Save the first block to t/s: 1/0
    ldx #1
    ldy #0
    jsr bitfire_save_write_block

    ;and the other three to 1/4, 1/8, 1/12
    jsr bitfire_save_write_next_block
    jsr bitfire_save_write_next_block
    jsr bitfire_save_write_next_block

    ;You may save other files here...

    ;You are done but the drive's motor is still running, so you call:
    jsr bitfire_save_finish

After the last jsr you can use Bitfire normally. Actually, the last jsr is 
optional, it only stops the drive's motor.

Error handling, and return codes:
 - bitfire_save_init:
   - X=0: all OK
   - X=1: write protected disk
 - bitfire_save_write_block, bitfire_save_write_next_block:
   - X=0: all OK
   - X=1: write protected disk
   - X=2: no sync
   - X=3: sector not found
   - X=4: write failed


Toubleshooting tips
^^^^^^^^^^^^^^^^^^^

- Don't try to load and decompress or decompress under ROM area when it is enabled.
- Make sure $04-$0a is only used by Bitfire while loading.
- If loading works with 1551 and doesn't work with 1541:
  - Make sure you don't ruin the resident part somehow ($200-$3ff), the 1551 
  version of the resident is full of NOPs to fill up gaps, so it may have 
  survived it. :)
  - Make sure you don't modify $00-$01.
  - Make sure you propery restore $ff13's clock bit before calling RTI.
  - Make sure you call 911, ie. report it back to us.
 

Binary Release
^^^^^^^^^^^^^^

The purpose of the binary release is to provide you with the compiled version of
Bitfire+4. You don't have to have Linux/Cygwin, etc. installed in order to start 
using the loader, but you can always download the source from GitHub and compile it
for yourself with different settings. The binary release contains all features of the
loader and it is still small, so you probably won't miss anything if you use it. ;)

The structure of the latest binary release:
 - acme/
   - acme.exe: ACME cross-assembler version 0.96.2
   - docs/: all the docs TXT files for ACME
 - bitfire/
   - bitnax.exe: packer based on doynax.
   - d64write.exe: image writer tool that can copy files to disk image in normal
       and Bitfire format.
   - installer_c64.prg: The original C64 version of Bitfire.
   - loader_*_c64.inc: include files contating all important routine and 
       memory addresses of the C64 loader.
   - installer_plus4_multi.prg: Installer for 1541/1551 with drive detection.
   - loader_*_plus4_multi.inc: include files contating all important routine and 
       memory addresses of the 1541/1551 loader.
   - installer_plus4_1551.prg: Inetaller for 1551 only.
   - loader_*_plus4_1551.inc: include file for the 1551 loader.
   - link_macros_*.inc: useful macros for many popular cross-assemblers
   - reset_drive.asm: reset drive routine for 1541/1551
   - request_disc.asm: request (next) disc routine for 1541/1551
 - save/
   - save_*.prg: precompiled binaries for each installers in the package, ie.:
     - C64 version
     - Plus/4 multi 1541/1551
     - Plus/4 1551 only  
   - save_*.inc: include files for all platforms
 - example/
   - main.asm: simple example demonstrating loader installation, loading raw files and 
       loading and depacking compressed files.
   - build.bat: simple build script demostrating how to compress files for Bitfire,
       how to create disc and copy files to it in normal and bitfire format.
   - bitmap*.prg: five bitmaps

Bitfire+4 2017.07.17:
 - Resident part: $200-$3ff
 - Zero page usage: $04-$0a
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
 - Zero page usage: $04-$0a
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
 
