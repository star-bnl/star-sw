#!/bin/sh
#-------------------------------------------------------
# Author: Valeri Fine (fine@bnl.gov)       2007/05/16
# $Id: install.csh,v 1.12 2008/08/28 20:44:58 fine Exp $
#-------------------------------------------------------
QT_VERSION=4.4.0
QTSDK=qt-x11-opensource-src-${QT_VERSION}
PLATFORM=g++
if [ "x$STARCMPL" == "xicc" ] ;  then
PLATFORM="icc -no-xmlpatterns -no-webkit " 
fi
# QTSDK=qt-x11-commercial-src-${QT_VERSION}
# --
# -- download the Qt4 kit
# 
if ! test -s ${QTSDK} 
then
  if ! test -s ${QTSDK}.tar.gz
  then
    echo 1. -- downloading the distribution kit --- 
     wget --quiet  http://wftp.tu-chemnitz.de/pub/Qt/qt/source/${QTSDK}.tar.gz
#    wget  --quiet ftp://ftp.trolltech.com/qt/source/${QTSDK}.tar.gz
  fi
# --
# -- upack the Qt4 kit
# 
  if  ! test -s ${QTSDK} 
  then
   echo 2. -- unzipping the distribution kit --- 
   tar -xzf ${QTSDK}.tar.gz
   #    rm ${QTSDK}.tar.gz
  fi
fi
# --
# -- configure the Qt4 to be installed under .$STAR_HOST_SYS/qt4
# 
if  test -s ${QTSDK} 
then
  cd ${QTSDK}
  echo 3. configure the Qt4 to be installed under .$STAR_HOST_SYS/qt4 from `pwd`
  echo Attn. the Qt4 -debug option creates both the debug as well as non-debug versions of the Qt4 binaries
#  ./configure -prefix-install -prefix .$STAR_HOST_SYS/${QT_VERSION}/debug -debug  -no-openssl
  ./configure -platform linux-${PLATFORM} -prefix-install -prefix .$STAR_HOST_SYS/${QT_VERSION} -fast -debug <<YES
yes
YES
# --
# -- build the Qt4 from `pwd`
#
echo "Compiling  ${QTSDK} and installing to  .$STAR_HOST_SYS/${QT_VERSION} . . . . Takes time ..."
make && make install 
else
  echo No Qt SDK ${QTSDK} has been found yet
  exit 1
fi


