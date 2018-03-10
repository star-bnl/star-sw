#!/bin/tcsh -f
#/star/u/kehw/hlt/vc.build/build.csh

#icc -v

# cd ../Vc
# git pull
# cd -

#cd ./build
if (! -d Vc) mkdir Vc
cd Vc
rm -rf ./*
if (! $?NODEBUG) then
    set ROOTBUILD="debug"
else
    set ROOTBUILD="opt"
endif
setenv LDFLAGS -m32
setenv VC_ROOT ../../Vc/
switch ( $STAR_HOST_SYS )  
    case *x8664_gcc7*:
	setenv VC_ROOT ../../Vc.1.3.3
    case *x8664*: 
	setenv LDFLAGS -m64
    breaksw
    default:
    breaksw
endsw 
#    -DCMAKE_C_COMPILER=`root-config --cc`                      \
#    -DCMAKE_CXX_COMPILER=`root-config --cxx`                  \
#    -DCMAKE_CXX_FLAGS=$LDFLAGS \

cmake -L                                        \
    -DCMAKE_BUILD_TYPE=$ROOTBUILD           \
    -DCMAKE_INSTALL_PREFIX=$XOPTSTAR    \
    -DBUILD_TESTING=OFF                          \
    -DCMAKE_C_FLAGS=\"`root-config --cflags`\" \
    ../../Vc/

make -j 10 install
touch ../Vc.Done
# make -j test
# make install
#    -DCMAKE_C_FLAGS=`root-config --auxcflags` \
#    -DCMAKE_CXX_FLAGS=`root-config --auxcflags` \
