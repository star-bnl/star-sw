* $Id: cterp.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CTERP(X1,X2,X,Y1,Y2,Y)
C       THIS ROUTINE PERFORMS LINEAR INTERPOLATION
      Y=Y2-(X2-X)*(Y2-Y1)/(X2-X1)
      RETURN
      END
