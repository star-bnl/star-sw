#
#Delete libraries, keep last NKeep
  @ NKeep = 10
  if ( $#argv ) @ NKeep = $1
  foreach LIB (*.s?)
    set List = ${LIB}.????
    @ NList = ${#List} 
    @ Ndel = $NList - $NKeep
    if ( $Ndel > 0) echo Clean ${LIB}: $List
    while ( $Ndel > 0 )
      echo rm $List[$Ndel] 
      rm $List[$Ndel] 
      @ Ndel = $Ndel - 1
    end
  end
  
