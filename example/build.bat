#Compressing files for loading with Bitfire
..\bitfire\bitnax.exe --bitfire -o bitmap1.lz bitmap1.prg
..\bitfire\bitnax.exe --bitfire -o bitmap2.lz bitmap2.prg
..\bitfire\bitnax.exe --bitfire -o bitmap3.lz bitmap3.prg
..\bitfire\bitnax.exe --bitfire -o bitmap4.lz bitmap4.prg
..\bitfire\bitnax.exe --bitfire -o bitmap5.lz bitmap5.prg
#Compiling the test code and creating an sfx
..\acme\acme.exe -f cbm -o main.prg main.asm
#Creating the disk and copying main.prg to it as a normal file
..\bitfire\d64write.exe -c disk.d64 -h hello -i world -s main.prg
#Copying files to the disk in Bitfire format
..\bitfire\d64write.exe -d disk.d64 -b bitmap1.prg -b bitmap2.prg -b bitmap3.prg -b bitmap4.prg -b bitmap5.prg
..\bitfire\d64write.exe -d disk.d64 -b bitmap1.lz  -b bitmap2.lz  -b bitmap3.lz  -b bitmap4.lz  -b bitmap5.lz
