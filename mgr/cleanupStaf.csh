#
set ARCH = `sys`
set ASPS = ( ami asu c4t dio dsl dsu dui lev msg pam sdd soc spx str tbr tdm tls tnt top )
if ( $#argv ) set ASPS = $1

foreach ASP ( $ASPS )
echo $ASP $ARCH
set LIB = .$ARCH/lib
if ( -d $LIB) then
  set QWE = $LIB/lib$ASP.a
  if ( -e $QWE[1] ) then
    echo rm -f $QWE
         rm -f $QWE
  endif
   set QWE = $LIB/$ASP.a
  if ( -e $QWE[1] ) then
    echo rm -f $QWE
         rm -f $QWE
  endif
 
  set QWE = $LIB/lib$ASP.s?.????
  if ( -e $QWE[1] ) then
    echo rm -f $QWE
         rm -f $QWE
  endif
  set QWE = $LIB/lib$ASP.s?
  if ( -e $QWE[1] ) then
    echo rm -f $QWE
         rm -f $QWE
  endif
  set QWE = $LIB/$ASP.s?.????
  if ( -e $QWE[1] ) then
    echo rm -f $QWE
         rm -f $QWE
  endif
  set QWE = $LIB/$ASP.s?
  if ( -e $QWE[1] ) then
    echo rm -f $QWE
         rm -f $QWE
  endif
endif

set OBJ = .$ARCH/obj/$ASP
if ( -e $OBJ ) then
  set QWE = $OBJ/*.o
  if ( -e $QWE[1] ) then
    echo rm -f $QWE
         rm -f $QWE
  endif
endif

set DEP = .$ARCH/dep/$ASP
if ( -e $DEP ) then
  set QWE = $DEP/*.d
  if ( -e $QWE[1] ) then
    echo rm -f $QWE
         rm -f $QWE
  endif
endif

set SRG = srg/$ASP
if ( -e $SRG ) then
  set QWE = $SRG/*.*
  if ( -e $QWE[1] ) then
    echo rm -f $QWE
         rm -f $QWE
  endif
endif

end


