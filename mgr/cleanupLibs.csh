#
#Delete libraries, keep last NKeep
  @ NKeep = 10
  if ( $#argv ) @ NKeep = $1
  foreach LIB (*.s?)
    echo Clean $LIB
    set List = ${LIB}.????
    @ NList = ${#List} 
    @ Ndel = $NList - $NKeep
    while ( $Ndel >= 0 )
      echo rm $List[$Ndel]
      @ Ndel = $Ndel - 1
    end
  end
  
