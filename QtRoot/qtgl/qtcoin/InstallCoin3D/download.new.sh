#!/bin/bash 
# This script downloads the Coin3d suite components 
# to back the ROOT Coin3D-based plugin
# See:  http://www.coin3d.org/lib/plonesoftwarecenter_view 

# stop at the first error.
# --trap  "echo ; echo SOME FATAL ERROR DURING Coin3D downloading, SORRY... ; echo ; exit;" ERR
# echo Downloading . . .  simage . . . .
# svn co https://svn.coin3d.org/repos/simacros/trunk/src simacros/src
# echo -- Done
# -------------  Coin3D ----------------
# svn co https://svn.coin3d.org/repos/Coin-2/trunk Coin-2
# svn co https://svn.coin3d.org/repos/Coin/trunk Coin-3


COIN_VERSION_MAJOR=3
COIN_VERSION_MINOR=1.1

SOQT_VERSION=-1.4.1
SIMAGE_VERSION=-1.6.1

SMALLCHANGE_VERSION=1083

#_____________________________________________________________________
Download() {
  # --  download the Coin3D $1 component, version $2
  package=$1
  package_version=$2
  full_package_file=${package}${package_version}
  echo Downloading . . .${full_package_file}  . . . .
  wget http://ftp.coin3d.org/coin/src/all/${full_package_file}.tar.gz
  # svn co https://svn.coin3d.org/repos/${package}/trunk ${package}
  if [ -d ${package} ]; then 
  # remove the old version to start from the scratch
      rm -rf ${package}
  fi
  tar -xzf ${full_package_file}.tar.gz
  #--  rename the directory (We can not use symbolic link to make Windows happy
  mv ${full_package_file} ${package}
  echo ------------ Done ! 
}
# --
# -------------  Coin ----------------
#
COIN_VERSION_MAJOR=3
COIN_VERSION_MINOR=1.1
COIN_VERSION=${COIN_VERSION_MAJOR}.${COIN_VERSION_MINOR}
PACKAGE=Coin-${COIN_VERSION_MAJOR}
PACKAGE_VERSION=.${COIN_VERSION_MINOR}
Download ${PACKAGE}  ${PACKAGE_VERSION}


# -- -r 2451
# -------------  SoQt ----------------
#
PACKAGE=SoQt
PACKAGE_VERSION=${SOQT_VERSION}
Download ${PACKAGE} ${PACKAGE_VERSION}

# --
# -------------  simage ----------------
#
PACKAGE=simage
PACKAGE_VERSION=${SIMAGE_VERSION}
Download ${PACKAGE} ${PACKAGE_VERSION} 
# --
# -------------  SmallChange from SVN repository ----------------
#
svn co https://svn.coin3d.org/repos/SmallChange/trunk  -r ${SMALLCHANGE_VERSION} SmallChange
echo " =========== Coin3D has been checked out ================ "
# svn co https://svn.coin3d.org/repos/Quarter/trunk Quarter
# svn co https://svn.coin3d.org/repos/SoGuiExamples/trunk SoGuiExamples
# svn co https://svn.coin3d.org/repos/simage/trunk simage
#wget http://ftp.coin3d.org/coin/src/all/Coin-2.4.6.tar.gz
#wget http://ftp.coin3d.org/coin/src/all/SoQt-1.4.1.tar.gz
