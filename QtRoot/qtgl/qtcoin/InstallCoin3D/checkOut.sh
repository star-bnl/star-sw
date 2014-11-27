#!/bin/sh
echo "Prepare Coin3D directory tree:"

mkdir Coin2
cd Coin2
mkdir srcDir
mkdir buildDir
cd srcDir

echo "Check Out Coin3D components from the CVS repository:"
echo "See: http://www.coin3d.org/doc/cvs_access"

# Coin abadoned the CVS and switched to the Subversion
# http://subversion.tigris.org/
# To install Coin fdrom the Copin3d Repository one 
# has to install the Subversion first

export COINCVSROOT=:pserver:cvs@cvs.coin3d.org:/export/cvsroot
export COINSVNROOT=https://svn.coin3d.org/repos
testPlatform=`uname | grep -c CYGWIN`
if test "x$testPlatform" != "x0" ; then
export PATH="`cygpath "E:/Program Files/svn-win32-1.4.2/bin"`":$PATH
echo $PATH
fi
# "Coin" - Latest development version
# cvs -z3 -d $COINCVSROOT checkout Coin

# "Coin-2" - Last stable production release
svn co ${COINSVNROOT}/Coin-2/trunk       Coin-2

svn co ${COINSVNROOT}/SmallChange/trunk  SmallChange
svn co ${COINSVNROOT}/simage/trunk       simage
svn co ${COINSVNROOT}/simacros/trunk     simacros
svn co ${COINSVNROOT}/SoQt/trunk         SoQt

svn co ${COINSVNROOT}/SoGuiExamples/trunk SoGuiExamples
