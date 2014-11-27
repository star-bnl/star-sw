#!/bin/bash 
# This script downloads the Coin3D suite components 
# to back the ROOT Coin3D-based plugin
# See:  http://www.coin3d.org/lib/plonesoftwarecenter_view 

# stop at the first error.
# --trap  "echo ; echo SOME FATAL ERROR DURING Coin3D downloading, SORRY... ; echo ; exit;" ERR

DOWNLOAD_FTP=Yes
COIN_FTP_SERVER=http://ftp.coin3d.org/coin/src/all
COIN_SVN_SERVER=https://svn.coin3d.org/repos

COIN_VERSION_MAJOR=3
COIN_VERSION_MINOR=1.3

# SoQt 1.4.1 is the last SoQt version available from the ftp server
# It is obsolete  and it is not compatible woth Coin-3
# use svn "trunk" to get the version compatible with Coin-3

SOQT_VERSION=-1.5.0

# simage 1.6.1 is the last simage version available from the ftp server
# It is obsolete  and it is not compatible woth Coin-3
# use svn "trunk" to get the version compatible with Coin-3

SIMAGE_VERSION=-1.7.0

SMALLCHANGE_VERSION=
SMALLCHANGE_SVN_REVISION=1083
# http://hg.sim.no/SmallChange/default/archive/tip.zip 
#_____________________________________________________________________
Download() {
  # --  download the Coin3D $1 component, version $2 or svn_revision $3
  package=$1
  package_version=$2
  svn_revision=$3
  full_package_file=${package}${package_version}
  
  if  [  "x$package_version"!="x" ]; then
    # ---   via ftp server
    if  [ "x$DOWNLOAD_FTP" == "xYes" ]; then
        if [ -d ${package} ]; then 
        # remove the old version to start from the scratch
            rm -rf ${package}
        fi
        echo Downloading . . .${full_package_file}  . . . .
        wget ${COIN_FTP_SERVER}/${full_package_file}.tar.gz
        echo Unpacking . . .${full_package_file}.tar.gz  . . . .
        tar -xzf ${full_package_file}.tar.gz
        #--  rename the directory (We can not use symbolic link to make Windows happy
        mv ${full_package_file} ${package}
    fi 
  fi  
  if  [ ! "x$DOWNLOAD_FTP" == "xYes" ]; then
    # ---   via svn server
    echo Downloading . . . ${package} from ${COIN_SVN_SERVER}/${package}/trunk   . . . .
    svn -q co ${COIN_SVN_SERVER}/${package}/trunk ${package}
  fi  
  echo ------------ Done ! 
}
# --
# -------------  Coin ----------------
#
COIN_VERSION=${COIN_VERSION_MAJOR}.${COIN_VERSION_MINOR}
PACKAGE=Coin-${COIN_VERSION_MAJOR}
PACKAGE_VERSION=.${COIN_VERSION_MINOR}
Download ${PACKAGE}  ${PACKAGE_VERSION}


# --
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
DOWNLOAD_FTP=No
# --
# -------------  SmallChange from SVN repository ----------------
#
PACKAGE=SmallChange
# PACKAGE_VERSION=${SMALLCHANGE_VERSION}
# SVN_REVISION=${SMALLCHANGE_SVN_REVISION}
# Download ${PACKAGE} ${PACKAGE_VERSION} 
wget http://hg.sim.no/${PACKAGE}/default/archive/tip.zip -O ${PACKAGE}.zip
unzip ${PACKAGE}.zip
mv default-* ${PACKAGE}

echo " =========== Coin3D has been checked out ================ "

# -- basement ;-)
# svn co https://svn.coin3d.org/repos/SmallChange/trunk  -r ${SMALLCHANGE_VERSION} SmallChange
# svn co https://svn.coin3d.org/repos/Quarter/trunk Quarter
# svn co https://svn.coin3d.org/repos/SoGuiExamples/trunk SoGuiExamples
# svn co https://svn.coin3d.org/repos/simage/trunk simage
# svn co https://svn.coin3d.org/repos/simacros/trunk/src simacros/src
# svn co https://svn.coin3d.org/repos/Coin-2/trunk Coin-2
# svn co https://svn.coin3d.org/repos/Coin/trunk Coin-3
