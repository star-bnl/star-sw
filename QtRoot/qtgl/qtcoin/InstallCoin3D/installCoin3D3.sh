#!/bin/sh
COIN_VERSION=3
IV_PLATFORM=$(uname -s )
if  [ `expr  match $IV_PLATFORM CYGWIN` == 6 ]; then
  IV_PLATFORM=Win
fi
echo "Installing Coin3D for $IV_PLATFORM platform . . . "
NCPUS=0
if [ "$IV_PLATFORM" == "Linux" ]; then
    NCPUS=$(grep -e 'cpu[0-9]' /proc/stat | grep -c .)
fi
##=================================================================
if [ "$IV_PLATFORM" == "Darwin" ]; then
    NCPUS=$(/usr/sbin/system_profiler SPHardwareDataType | grep Cores: | sed s/.*Cores://g )
fi
##=================================================================
if [ "$IV_PLATFORM" == "Win" ]; then
    NCPUS=$(grep -e 'cpu[0-9]' /proc/stat | grep -c .)
	# check TEMP variable
	if [ "x$TEMP" == "x" ]; then
	    if [ ! -d /usr/tmp ]; then
	        mkdir -p /usr/tmp
	    fi
	    export TEMP=/usr/tmp
	fi
fi

if [ "$NCPUS" -le "0" ]; then
    NCPUS=1;
fi

# stop at the first error.
# trap  "echo ; echo SOME FATAL ERROR DURING COIN3D INSTALLATION, SORRY... ; echo ; exit;" ERR

# CoinInstallDir 
srcdir=`pwd`
platform=$STAR_HOST_SYS

if test "x$platform" = "x" ; then
   platform=`uname`
fi 

packageName=$platform/coin3d-${COIN_VERSION}

cd ..
builddirbase=`pwd`/buildDir${COIN_VERSION}
installDir=$1
if test ! $installDir;  then
  echo "** Error ** missing installDir. Please specify!"
  echo ""
  echo "cd to the directory where \"cvs co\" has been performed"
  echo ""
  echo "Usage: install installDir [debug]"
  echo "-----"
  exit 1
fi

if !(test -d "$installDir"); then
  echo "** Warning ** \"$installDir\" does not exist. Create it!"
  if !(mkdir -p $installDir); then
     exit 1
  fi
fi

if !(test -d "$builddirbase"); then
  echo "** Warning ** \"$builddirbase\" does not exist. Create it!"
  if !(mkdir -p $builddirbase); then
     exit 1
  fi
fi

installDir=`pwd`/$installDir/$packageName
echo "Coin package will be installed in to the \"$installDir\" directory from $srcdir"
builddir=$builddirbase/${packageName}

cd $srcdir
if [ -d Coin-${COIN_VERSION} ]; then 
 
# define Windows option if any
 testPlatform=`uname | grep -c CYGWIN`
 if test "x$testPlatform" = "x1" ; then
   msvcrt=--with-msvcrt=multithread-dynamic
   if test "x$2" = "xdebug" ; then
      msvcrt=--with-msvcrt=multithread-dynamic-debug
      enable_qt_debug=--enable-qt-debug
   fi
 fi
 
 common_build_opt="--prefix=$installDir  $msvcrt --enable-threadsafe  --enable-debug --disable-dependency-tracking"

 cd $builddirbase
 mkdir -p ${packageName}
 cd ${packageName}
  if [ ! -d simage ];  then
     mkdir simage
  fi
  if [ ! -d Coin-${COIN_VERSION} ];  then
     mkdir Coin-${COIN_VERSION}
  fi
  if [ ! -d SmallChange ];  then
     mkdir SmallChange
  fi
  if [ ! -d SoQt ];        then
     mkdir SoQt
  fi
#  if [ ! -d Quarter ];        then
#     mkdir Quarter
#  fi
  if [ ! -d SoGuiExamples-SoQt ];  then
     mkdir SoGuiExamples-SoQt
  fi
 cd ../..
 pwd
 echo " Configure simage $builddir/simage"
 cd $builddir/simage
  $srcdir/simage/configure --enable-optimization=yes  $enable_qt_debug --enable-qimage --with-qt=true --with-mpeg2enc --with-avienc $common_build_opt
  make -j $NCPUS
  make install
 
 cd $builddir/Coin-${COIN_VERSION}
 echo " Configure Coin3d  at `pwd`"
 pwd

  $srcdir/Coin-${COIN_VERSION}/configure  --enable-optimization=yes  $common_build_opt
  make -j $NCPUS
  make install

 cd $builddir/SmallChange
 echo " Configure Coin3d  at `pwd`"
 pwd

  $srcdir/SmallChange/configure --enable-optimization=yes  $common_build_opt
  make -j $NCPUS
  make install
  
 echo " Configure SoQt"
 cd $builddir/SoQt
   $srcdir/SoQt/configure   $enable_qt_debug  --with-qt=true  --with-coin    $common_build_opt
    make -j $NCPUS
    make install
  
# echo " Configure Quarter"
# cd $builddir/Quarter
#   $srcdir/Quarter/configure   $enable_qt_debug  --with-qt=true  --with-coin    $common_build_opt
#    make install

  if test "x$testPlatform" = "x0" ; then
    # non Windows 
   echo " Configure SoXt"
   echo " Do NOT build SoXt!!!  It may confuse the ROOT configure utility!!!"

  # cd $builddir/SoXt
  #   $srcdir/SoXt/configure  --with-coin --enable-debug --enable-threadsafe
  #    make install
  fi

  export PATH=$installDir/bin:$PATH
# cd $builddir/SoGuiExamples-SoQt
## Skip this step on Windows 
#  uname -s | grep CYGWIN && PLATFORM=Win
#  if [ "x$PLATFORM" != "xWin" ]; then 
#    $srcdir/SoGuiExamples/configure --with-soqt $common_build_opt
#    make
#  fi
  echo "---- Installation of Coin3D package has been completed"
  echo " Do not forget to set the env variable "
  echo "export IVROOT=$installDir"
else
  echo "** Error **  There is no Coin-${COIN_VERSION} src directory yet. Check Coin2 package out from \":pserver:cvs@cvs.coin3d.org:/export/cvsroot\" "
fi
  
