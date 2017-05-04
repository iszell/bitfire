Bitfire+4
^^^^^^^^^

Credit:
 - Original C64 loader: Bitbreaker/Oxyron
 - Plus/4 port: Bubis/Resource, BSZ/NST
 - Testing: Luca/FIRE, Chronos/ACE, Csaba/New Wave

GitHub:
 - http://github/dotscha/bitfire

What is this?
^^^^^^^^^^^^^

This is a Plus/4 port of Bitfire. Bitfire is a modern IRQ loader for C64 with it's own
packer and BAM compatible file format that has a really small resident code. Please read
readme.txt for further details.

Differences from the C64 version
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First of all, Bitfire+4 can be compiled to both platforms, this is driven by the
BITFIRE_PLATFORM constant in bitfire/config.inc. If you set BITFIRE_C64 there you can
compile original C64 version of the loader (it is set to BITFIRE_PLUS4 by default).
A few things just doesn't make sense on Plus/4, like the NMI gap, link_load_next_double, 
link_decomp_under_io, CIA detection. You won't find those in the Plus/4 build.
The SID base address is not obvious on Plus/4, you have to change BITFIRE_SID if that is
not $FD40 for you. ($FD40 is recommended of course.) The type of the SID chip is detected
correctly by the installer but missing SID is not detected yet, this will be improved
later.
Bitnax has a new option --plus4 that will create a Plus/4 executable when used with
the --sfx <addr> option.

Protocols:
^^^^^^^^^^

2bit ATN (double clock receiver):

This is the original protocol, the receiver code does not switch between single clock
and double clock and does not disable irqs. The code is optimized for double clock and
about as fast as the C64 protocol when running on the border. The obvious advantage of
this receiver is that you don't have to worry about the timing of your interrupts. The
disadvantage is that transfer is slow when running on the screen.

2bit ATN (single clock receiver):

This is not implemented yet, but will be comming soon. The motivation is to have a 
receiver that is equaly fast on the screen and the border. The disadvantege of this is 
that it is harder to write interrupt routines that work equally well regardless of single
clock or double clock mode. If you have a simple loader image on the screen and an irq 
for the music, this is the ideal receiver for you.

8bit 1551 protocol:

It will be implemented later and it will be very fast! Any questions? :)

Protocol switching:

We are planning to add optional routines for switching between the single/double
clock receivers. 

Binary Releases:
^^^^^^^^^^^^^^^^

Bitfire+4 2017.05.04:
 - Resident part: $200-$3ff
 - Zero page: $04-$08
 - 2bit ATN double clock receiver
 - load raw, load+decomp, decomp routines
 - Basic irq handler for music, frame counter, partial SID detection
 - Win32 and Plus/4 binaries, ACME cross-assembler, include files and macros
 - Simple code example and Win32 build script.

