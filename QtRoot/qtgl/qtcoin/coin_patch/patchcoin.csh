#!/usr/local/bin/tcsh
cd $IVROOT
cd ../srcDir/Coin-2/src
setenv  COINPATCHDIR $STAR/QtRoot/qtgl/qtcoin/coin_patch
echo Patch the Coin  $IVROOT from ${COINPATCHDIR}
patch -b -p0 <${COINPATCHDIR}/Coin2.4.5.diff
