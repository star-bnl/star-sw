#
set ARCH = `sys`
set LIB = .$ARCH/lib
if ( -d $LIB) then
  echo rm -f $LIB/*
  rm -f $LIB/*
endif

set OBJ = .$ARCH/obj
if ( -e $OBJ ) then
  cd $OBJ
  foreach dir (???)
  if ( $dir != "exe") then
    echo rm -rf $cwd/$dir
    rm -rf $dir
  endif
  end
  cd ../../
endif

set DEP = .$ARCH/dep
if ( -e $DEP ) then
  cd $DEP
  foreach dir (???)
  if ( $dir != "exe") then
    echo rm -rf $cwd/$dir
    rm -rf $dir
  endif
  end
  cd ../../
 endif

set SRG = srg
if ( -e $SRG ) then
  cd $SRG
  foreach dir (???)
  if ( $dir != "exe") then
    echo rm -rf $cwd/$dir
    rm -rf $dir
  endif
  end
 cd ../../
 endif




