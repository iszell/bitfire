# You can specify disk header and id
YOUR_HEADER="0123456789abcdef"
YOUR_ID="abcde"

# Or a dir art file that is a simple screen dump
DIRART_FILE=dirart.prg
DIRART_HEIGHT=17

# Start with loading 1 or 2 compressed files 
LOADCOMP=2

# Then jump to this address
START_ADDR=2000

# Set 64 or 16 for c64 or plus/4 accordingly.
PLATFORM=16


acme -f cbm -o boot.prg -DLOAD=$LOADCOMP -DSTART=0x$START_ADDR -DPLATFORM=$PLATFORM boot.asm
if [ $PLATFORM == 16 ]
then
  ../bitfire/bitnax --sfx 4352 --plus4 -o boot boot.prg
fi
if [ $PLATFORM == 64 ]
then
  ../bitfire/bitnax --sfx 4352 -o boot boot.prg
fi

../bitfire/d64write -c boot_load$LOADCOMP_g$START_ADDR.d64 -h $YOUR_HEADER -i $YOUR_ID --side 1 --boot boot
../bitfire/d64write -c boot_dirart_load$LOADCOMP_g$START_ADDR.d64 -a $DIRART_HEIGHT $DIRART_FILE --side 1 --boot boot

rm boot boot.prg
