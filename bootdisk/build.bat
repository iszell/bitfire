:: You can specify disk header and id
@set YOUR_HEADER="0123456789abcdef"
@set YOUR_ID="abcde"

:: Or a dir art file that is a simple screen dump
@set DIRART_FILE=dirart.prg
@set DIRART_HEIGHT=17

:: Start with loading 1 or 2 compressed files 
@set LOADCOMP=2

:: Then jump to this address
@set START_ADDR=2000

:: Set 64 or 16 for c64 or plus/4 accordingly.
@set PLATFORM=16



..\acme\acme.exe -f cbm -o boot.prg -DLOAD=%LOADCOMP% -DSTART=0x%START_ADDR% -DPLATFORM=%PLATFORM% boot.asm
if %PLATFORM% equ 16 (
  ..\bitfire\bitnax.exe --sfx 4352 --plus4 -o boot boot.prg
)
if %PLATFORM% equ 64 (
  ..\bitfire\bitnax.exe --sfx 4352 -o boot boot.prg
)
..\bitfire\d64write.exe -c boot_load%LOADCOMP%_g%START_ADDR%.d64 -h %YOUR_HEADER% -i %YOUR_ID% --side 1 --boot boot
..\bitfire\d64write.exe -c boot_dirart_load%LOADCOMP%_g%START_ADDR%.d64 -a %DIRART_HEIGHT% %DIRART_FILE% --side 1 --boot boot

@del boot boot.prg

@pause
