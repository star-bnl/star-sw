#!/bin/tcsh -f
#/star/u/kehw/hlt/vc.build/build.csh

#icc -v

# cd ../Vc
# git pull
# cd -

#cd ./build
rm -rf ./*
if (! $?NODEBUG) then
    set ROOTBUILD="debug"
else
    set ROOTBUILD="opt"
endif
setenv LDFLAGS -m32
switch ( $STAR_HOST_SYS )  
    case *x8664*: 
	setenv LDFLAGS -m64
    breaksw
    default:
    breaksw
endsw 
cmake -L                                        \
    -DCMAKE_BUILD_TYPE=$ROOTBUILD           \
    -DCMAKE_INSTALL_PREFIX=$ROOTSYS    \
    -DCMAKE_C_COMPILER=`root-config --cc`                      \
    -DCMAKE_CXX_COMPILER=`root-config --cxx`                  \
    -DBUILD_TESTING=OFF                          \
    -DCMAKE_C_FLAGS=$LDFLAGS \
    -DCMAKE_CXX_FLAGS=$LDFLAGS \
    ../../Vc/

make -j 10 install
touch ../Vc.Done
# make -j test
# make install
#    -DCMAKE_C_FLAGS=`root-config --auxcflags` \
#    -DCMAKE_CXX_FLAGS=`root-config --auxcflags` \
