# $Id: cleanupLibs.csh,v 1.5 1999/02/23 01:07:12 fisyak Exp $
# $Log: cleanupLibs.csh,v $
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
    @ Ndel = $NList - $NKeep
    if ( $Ndel > 0) echo Clean ${LIB}: $List
    while ( $Ndel > 0 )
      echo rm $List[$Ndel] 
      rm $List[$Ndel] 
      @ Ndel = $Ndel - 1
    end
  end
  
