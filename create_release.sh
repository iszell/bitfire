rm -rf release
make -C bitfire clean all
BF=release/bitfire
AS=release/acme
SV=release/save
mkdir -p $AS
mkdir -p $BF
mkdir -p $SV
mkdir -p $SV/src
cp $HOME/bin/acme $HOME/tools/acme/acme.exe $AS
cp -r $HOME/tools/acme/docs $AS
cp bitfire/*.exe $BF
cp bitfire/bitnax $BF
cp bitfire/d64write $BF
cp bitfire/installer_c64.prg $BF
cp bitfire/loader_*_c64.inc $BF
cp bitfire/installer_plus4_multi*.prg $BF
cp bitfire/loader_*_plus4_multi*.inc $BF
cp bitfire/installer_plus4_1551.prg $BF
cp bitfire/loader_*_plus4_1551.inc $BF
cp link_macros_*.inc $BF
cp bitfire/request_disc.asm bitfire/reset_drive.asm $BF
cp bitfire/save*.prg bitfire/save_*.inc $SV
cp bitfire/drivecode*.inc bitfire bitfire/sector_routines.asm $SV/src
cat bitfire/save.asm | sed "s/loader/..\/..\/bitfire\/loader/g" > $SV/src/save.asm
cp save_build.bat $SV/src/build.bat
cp changelog readme*.txt release
cp -r example release/
cp -r bootdisk release/
rm release/example/build.sh
cd release
zip -9 -r ../bitfire_plus4_`date +%Y%m%d`.zip .
