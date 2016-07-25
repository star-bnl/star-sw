#! /usr/local/bin/tcsh -x
setenv STAR  STAR
setenv ROOTBUILD debug
setenv PYTHIA /afs/usatlas/offline/external/ROOT/XNew/.$SystemName/root
setenv PYTHIA6 /afs/usatlas/offline/external/ROOT/XNew/.$SystemName/root
setenv VENUS /afs/usatlas/offline/external/ROOT/XNew/.$SystemName/root
setenv SystemName rh61_gcc2952
switch ( $SystemName )  
    case i386*:
    case rh*:
	setenv ARCH  linuxegcs
    breaksw
    case sun4x_5*:
	setenv ARCH solarisCC5
    breaksw
    case hp_ux102:
	setenv ARCH hpuxacc
	setenv XPM $ROOTSYS/lib
    breaksw
    default:
     exit 1
endsw
switch ( $SystemName)  
    case hp_ux102:
    unsetenv MYSQL
./configure $ARCH \
    --prefix=$ROOTSYS \
    --datadir=$ROOTSYS \
    --etcdir=$ROOTSYS/etc \
    --cintincdir=$ROOTSYS/cint \
    --incdir=$ROOTSYS/include \
    --with-ttf-incdir=/usr/local/include \
    --with-ttf-libdir=/usr/local/lib \
    --with-cern-libdir=/cern/pro/lib \
    --with-afs=/usr/awsfs/lib 
    breaksw
    case sun4x_5*:
    setenv MYSQL /afs/rhic/sun4x_56/opt/star/lib/
./configure $ARCH \
    --enable-afs --enable-thread --enable-ttf \
    --prefix=$ROOTSYS \
    --datadir=$ROOTSYS \
    --etcdir=$ROOTSYS/etc \
    --incdir=$ROOTSYS/include \
    --cintincdir=$ROOTSYS/cint 
    breaksw
    default:
#    setenv MYSQL /afs/usatlas.bnl.gov/offline/external/MySQL/3.23.32
#    setenv MYSQL /afs/cern.ch/atlas/offline/external/MySQL/4.0.2/Linux
    setenv MYSQL /afs/cern.ch/atlas/offline/external/MySQL/3.23.48/Linux
./configure $ARCH \
    --prefix=$ROOTSYS \
    --datadir=$ROOTSYS \
    --etcdir=$ROOTSYS/etc \
    --incdir=$ROOTSYS/include \
    --cintincdir=$ROOTSYS/cint \
    --with-ttf-incdir=/usr/local/include \
    --with-ttf-libdir=/usr/local/lib \
    --with-cern-libdir=/cern/pro/lib \
    --with-shift-libdir=/usr/local/lib \
    --with-thread-libdir=/usr/lib \
    --with-afs=/usr/awsfs/lib 
endsw



