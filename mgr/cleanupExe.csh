#
set ARCH = `sys`

set DIR = .$ARCH/obj/exe
if ( -e $DIR ) then
  echo rm -rf $cwd/$DIR/*
  rm -rf $DIR/*
endif

set DIR = .$ARCH/dep/exe
if ( -e $DIR ) then
  echo rm -rf $cwd/$DIR/*
  rm -rf $DIR/*
endif

set DIR = srg/exe
if ( -e $DIR ) then
  echo rm -rf $cwd/$DIR/*
  rm -rf $DIR/*
endif

set DIR = .$ARCH/bin
if ( -e $DIR ) then
  echo rm -rf $cwd/$DIR/Staf
  rm -rf $DIR/Staf
endif

set DIR = .$ARCH/dep
if ( -e $DIR ) then
  echo rm -rf $cwd/$DIR/*.d
  rm -rf $DIR/*.d
endif
