#
# This mmacro is a temmporary  solution. It ust be the part of "cons"
# All the *pcm fules creared by Cling must be linked to $STARSYS/lib
# When this will be done this file will be remmoved.
#V.Perev


set STAR_OBJ = ${STAR}/.${STAR_HOST_SYS}/obj
cd $STAR_OBJ
set DIRS = (StRoot StarVMC StDb asps pams)

foreach d ($DIRS)
##  ls $d
##	pcm's
  set pcm = (`find $d -name '*Cint_rdict.pcm'`)
  foreach p ($pcm)
    set locName = $p

    set gloName = ${locName:t}
    set gloName = ${STAR_LIB}/${gloName}
    if (!(-e "$gloName")) then
      rm -f $gloName
      ln -s $STAR_OBJ/$locName $gloName
    endif
  end
##  	Libraies
  set pcm = (`find $d -name '*.so'`)
  foreach l ($pcm)
    set o = ${l:t}
    set o = ${STAR_LIB}/${o}
    if (-e "$o") continue
    rm -f $o
    ln -s ${STAR_OBJ}/$l $o
  end
end
