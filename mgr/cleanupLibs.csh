# $Id: cleanupLibs.csh,v 1.6 1999/05/10 19:43:38 fisyak Exp $
# $Log: cleanupLibs.csh,v $
# Revision 1.6  1999/05/10 19:43:38  fisyak
# Add Victor test for last library
#
# Revision 1.5  1999/02/23 01:07:12  fisyak
# Cleanup for SL99a
#
# Revision 1.4  1999/01/09 21:30:17  fisyak
# By default keep only 2 last libraries instead 10
#
#Delete libraries, keep last NKeep
  @ NKeep = 1
  if ( $#argv ) @ NKeep = $1
  foreach LIB (*.s?)
    set List = `ls ${LIB}.????`
    @ NList = ${#List} 
    set last = $List[${NList}]
#   echo LAST = $last
    @ Ndel = $NList - $NKeep
    if ( $Ndel > 0) echo Clean ${LIB}: $List
    while ( $Ndel > 0 )
      echo rm $List[$Ndel] 
      rm $List[$Ndel] 
      @ Ndel = $Ndel - 1
    end
    rm -f ${LIB}
    echo    ln -sf $last ${LIB}
    ln -sf $last ${LIB}
  end
  
