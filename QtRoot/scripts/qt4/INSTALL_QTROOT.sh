#!/bin/bash 

#######################################################
#
# This macro tries to install a complete ROOT + QT + QTROOT(cvs) + COIN3D
# build environment.
#
# Original instructions here: http://root.bnl.gov/QtRoot/How2Install4Unix.html
#
# the **COMPLETE** stuff is installed in the current working directory.
# so you can also test different versions if you feel adventurous...
#
# you will need curl and/or wget to download the files.
# you should read also ALL the various licenses.
#
# about 100' on a AMD 3800+, ~3 Gb disk space.
#
#######################################################
# Author: L.Bardelli [ bardelli@fi.infn.it ]
#######################################################

# the versions we will use:
QT_VERSION=4.3.4
ROOT_VERSION=5.18.00

#######################################################

# stop at the first error.
trap  "echo ; echo SOME FATAL ERROR DURING EXECUTION, SORRY... ; echo ; exit;" ERR


## http://en.wikipedia.org/wiki/Uname
PLATFORM=$(uname -s )
uname -s | grep CYGWIN && PLATFORM=Win



if [ "$PLATFORM" != "Linux" ]; then
   if [ "$PLATFORM" != "Darwin" ]; then
      echo "This macro is not tested outside Linux or MacOSX! Good luck..."
      sleep 10;
   fi
fi


##=================================================================
if [ "$PLATFORM" == "Linux" ]; then
    QT_PLATFORM=x11
    NCPUS=$(grep -e 'cpu[0-9]' /proc/stat | grep -c .)
fi
##=================================================================
if [ "$PLATFORM" == "Darwin" ]; then
    QT_PLATFORM=mac
    QT_CONF_OPTS=-no-framework
    ROOT_PLATFORM=maxosx
    NCPUS=$(/usr/sbin/system_profiler SPHardwareDataType | grep Cores: | sed s/.*Cores://g )
    QMAKESPEC=macx-g++
fi
##=================================================================
if [ "$PLATFORM" == "Win" ]; then
    QT_PLATFORM=win
fi
##=================================================================


if [ "$NCPUS" -le "0" ]; then
    NCPUS=1;
fi
if [ "$NCPUS" -ge "4" ]; then
    NCPUS=4;
fi

##=================================================================
## this is THE working directory where EVERYTHING will go.
MYWD=$(pwd)

export QTDIR=$MYWD/Qt-$(echo $QT_VERSION)/

export ROOTSYS=$MYWD/root
export QTROOTSYSDIR=$ROOTSYS

export LD_LIBRARY_PATH=$ROOTSYS/lib:$QTDIR/lib:$LD_LIBRARY_PATH
export PATH=$ROOTSYS/bin:$QTDIR/bin:$PATH

##=================================================================


echo "========================================="
echo "(L.B.)"
echo "This will compile and install QT+ROOT(+COIN3D) in the current directory!!"
echo ""
echo "Current config: Qt v.$QT_VERSION opensource, ROOT v.$ROOT_VERSION"
echo "                dir=$MYWD"
echo "                PLATFORM=$PLATFORM, make will use $NCPUS cpu(s)"
echo ""
echo "It will require ~1-2 hours and ~3 Gb on disk. Current dir disk space is:"
df -h $MYWD 
echo "========================================="
echo ""
read -p "Do you want to proceed? (yes/no) " ANS

if [ "$ANS" != "yes" ]; then
    exit 0;
fi

read -p "Do you want to install COIN3D also? (yes/no) " USE_coin
if [ "$USE_coin" == "yes" ]; then
        ## no ending "/" here!!!
        export IVROOT=$MYWD/coin3d
	export LD_LIBRARY_PATH=$IVROOT/lib:$LD_LIBRARY_PATH
fi





echo ""
echo ">>>>> COMPILATION STARTS. Be patient..."
sleep 3 ## last chance for CTRL+C




## wget or curl? I prefer wget
GET="wget -c"
which wget || GET="curl -C - -O "


## ============= DOWNLOADS ==========================================
## with ls I check than the "true" file exists, and not a useless
## redirect from the server (like maintenance of similar...)

QTPKG=qt-$(echo $QT_PLATFORM)-opensource-src-$(echo $QT_VERSION).tar.gz

$GET ftp://ftp.trolltech.com/qt/source/$QTPKG
ls -lh $QTPKG

$GET ftp://root.cern.ch/root/root_v$(echo $ROOT_VERSION).source.tar.gz
ls -lh root_v$(echo $ROOT_VERSION).source.tar.gz

##OLDTAR $GET http://root.bnl.gov/QtRoot/downloads/qtFullRoot.tar.gz
##OLDTAR ls -lh qtFullRoot.tar.gz

cvs -d :pserver:cvsuser:cvsuser@cvs.bnl.gov:/data01/CVS login
cvs -d :pserver:cvsuser@cvs.bnl.gov:/data01/CVS co -Pd qtRoot root



## ============= ENVIRONMENT.sh =====================================
echo "#!/bin/bash" > set_environment.sh
echo export QTDIR=$QTDIR >> set_environment.sh
echo export QMAKESPEC=$QMAKESPEC >> set_environment.sh
echo export QT_CONF_OPTS=$QT_CONF_OPTS >> set_environment.sh
echo export ROOTSYS=$ROOTSYS >> set_environment.sh
echo export IVROOT=$IVROOT >>  set_environment.sh
echo export QTROOTSYSDIR=$QTROOTSYSDIR >> set_environment.sh
echo export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:\$LD_LIBRARY_PATH>> set_environment.sh
echo export PATH=$ROOTSYS/bin:$QTDIR/bin:\$PATH>> set_environment.sh

chmod -x  set_environment.sh ## user must "source" and not exec!


## ============= ENVIRONMENT.csh =====================================
cat set_environment.sh | sed s/=/" "/g | sed s/^export/set/g | sed s_"/bin/bash"_"/bin/csh"_g > set_environment.csh


## ============= for MAC-OSX: environment.plist ======================
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?> 
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\"> 
<plist version=\"1.0\"> 
<dict> 
        <key>DYLD_LIBRARY_PATH</key> 
        <string>$ROOTSYS/lib</string> 
        <key>QTROOTSYSDIR</key> 
        <string>$ROOTSYS</string> 
        <key>ROOTSYS</key> 
        <string>$ROOTSYS</string> 
</dict> 
</plist> " > environment.plist

##===================== QT ==========================
cd $MYWD
if [ ! -f done.qt ]; then
    rm -rf $QTDIR
    tar xzvf qt-$(echo $QT_PLATFORM)-opensource-src-$(echo $QT_VERSION).tar.gz    
    cd qt-$(echo $QT_PLATFORM)-opensource-src-$(echo $QT_VERSION)
    echo yes | ./configure --prefix=$QTDIR $QT_CONF_OPTS
    make -j $NCPUS
    make install
    cd $MYWD
    touch done.qt 
fi


##===================== ROOT ==========================
cd $MYWD
if [ ! -f done.root ]; then
    rm -rf root
    tar xzvf root_v$(echo $ROOT_VERSION).source.tar.gz
    cd root

## note1: DON'T use --prefix!
## note2: mac seems to need "./configure macosx" to work properly...

    ./configure $ROOT_PLATFORM --enable-table
    make -j $NCPUS
    make install
    cd $MYWD
    touch done.root
fi
##=====================coin3d=============================
if [ "$USE_coin" == "yes" ]; then

    cd $MYWD
    if [ ! -f done.coin ]; then
	rm -f  done.qtroot
	cd qtRoot/qtgl/qtcoin/InstallCoin3D/

## server may have outdated certificate. (t)emporarily accept it.
    echo "t
t
t
t
t
t
t
t
t
t " | ./download.sh
    ./installCoin3D.sh coin3d

# use more simple names.
    rm -f $IVROOT
    ln -sf $MYWD/qtRoot/qtgl/qtcoin/coin3d/$(echo $PLATFORM)/coin3d $IVROOT
    cd $MYWD
    touch done.coin3d
    fi
    fi

##======================  QT-ROOT (2/2) ==========================
cd $MYWD
if [ ! -f done.qtroot ]; then
    cd qtRoot
    qmake
    make -j $NCPUS
    make install 
    cd $MYWD
    if [ "$USE_coin" == "yes" ]; then
      ln -f -s $MYWD/qtRoot/qtgl/qglviewer/QGLViewer/libQGLViewer* $ROOTSYS/lib/
    fi
    touch  done.qtroot
fi


cd $MYWD
ROOTRC=root/etc/system.rootrc
echo "" >>  $ROOTRC
echo "## added by INSTALL_QTROOT.sh ##" >>  $ROOTRC
echo "" >>  $ROOTRC
cat qtRoot/qtExamples/QtGBrowser/rootrcqtgui  >> root/etc/system.rootrc


##===================== example test ======================
cd $MYWD
cd qtRoot/qtExamples/HelloCanvas
qmake
make

##====================== the end ==========================
cd $MYWD
echo ""
echo "====================== DONE! ========================="
echo ""
echo "The local ROOT copy is QT-enabled (patched rootrc)"
echo ""
if [ "USE_coin" == "yes" ]; then
    echo "you can use qtRoot/qtExamples/macros/rootgeom_coin.C to test COIN3D"
    echo ""
fi
echo "environment variables saved into $MYWD/set_environment.sh"
echo "            use it with:  source $MYWD/set_environment.sh"
echo "            or append it to your .bash_profile"
echo ""
echo "if your shell is csh use $MYWD/set_environment.csh"
echo ""
if [ "$PLATFORM" == "Darwin" ]; then
## mac-specific warnings
    echo "  MAC-OSX: you should now create a folder in your home named .MacOSX"
    echo "           and copy there the file environment.plist"
    echo ""
fi
echo "====================== BYE BYE! ======================"
echo ""
