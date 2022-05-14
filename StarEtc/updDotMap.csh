#
# This mmacro is a temmporary  solution. It ust be the part of "cons"
# All the *pcm fules creared by Cling must be linked to $STARSYS/lib
# When this will be done this file will be remmoved.
#V.Perev

set STAR_OBJ = $STARSYS/obj
set DIRS = ($STAR_OBJ/StRoot $STAR_OBJ/StarVMC $STAR_OBJ/StDb $STAR_OBJ/asps $STAR_OBJ/pams)
set LDP = $LD_LIBRARY_PATH
foreach d ($DIRS)
##  ls $d
##	pcm's
  set map = (`find $d -name '*.rootmap'`)
  foreach p ($map)
##echo Mapfile = $p
    set ld = (":${LDP}:")
    set x = (`echo ${ld} | grep ":${p}:"`)
    if (${#x}) continue
    set LDP = "${LDP}:${p}"
##    echo BOT = ${LDP}
  end
end
setenv LD_LIBRARY_PATH $LDP
