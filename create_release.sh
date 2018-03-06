rm -rf release
BF=release/bitfire
AS=release/acme
SV=release/save
BD=release/bootdisk
mkdir -p $AS
mkdir -p $AS/docs
mkdir -p $BF
mkdir -p $SV
mkdir -p $SV/src
mkdir -p $BD
cp ../acme/src/acme.exe $AS
cp ../acme/docs/* $AS/docs
cp bitfire/*.exe $BF
cp bitfire/installer_c64.prg $BF
cp bitfire/loader_*_c64.inc $BF
cp bitfire/installer_plus4_multi.prg $BF
cp bitfire/loader_*_plus4_multi.inc $BF
cp bitfire/installer_plus4_1551.prg $BF
cp bitfire/loader_*_plus4_1551.inc $BF
cp link_macros_*.inc $BF
cp bitfire/request_disc.asm bitfire/reset_drive.asm $BF
cp bitfire/save*.prg bitfire/save_*.inc $SV
cp bitfire/drivecode_acme_*.inc bitfire bitfire/sector_routines.asm $SV/src
cat bitfire/save.asm | sed "s/loader/..\/..\/bitfire\/loader/g" > $SV/src/save.asm
cp build_save.bat $SV/src/build.bat
cp changelog readme*.txt release
cp -r example release/
cp bootdisk/boot.asm bootdisk/build.bat bootdisk/dirart.prg $BD
rm release/example/build.sh
cd release
zip -9 -r ../bitfire_plus4_`date +%Y%m%d`.zip .
