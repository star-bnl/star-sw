#
set ARCH = `sys`
set ASPS = ( ami asu c4t dio dsl dsu dui lev msg pam sdd soc spx str tbr tdm tls tnt top )
if ( $#argv ) set ASPS = $1

foreach ASP ( $ASPS )
echo $ASP $ARCH
set LIB = .$ARCH/lib
if ( -d $LIB) then
  echo rm -f $LIB/lib$ASP.a
  rm -f $LIB/lib$ASP.a
  echo rm -f $LIB/lib$ASP.s?.????
  rm -f $LIB/lib$ASP.s?.????
endif

set OBJ = .$ARCH/obj/$ASP
if ( -e $OBJ ) then
  echo rm -rf $OBJ/*.o
  rm -rf $OBJ/*.o
endif

set DEP = .$ARCH/dep/$ASP
if ( -e $DEP ) then
  echo rm -rf $DEP/*.d
  rm -rf $DEP/*.d
endif

set SRG = srg/$ASP
if ( -e $SRG ) then
  echo rm -rf $SRG/*.*
  rm -rf $SRG/*.*
endif

end


