* $Id: cdost.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION CDOST(I,Z)
C
#include "gcalor/cevcm.inc"
C
      IF(Z-70.0) 30,10,10
   10 CDOST = T(I,7)
   20 RETURN
   30 IF(Z-10.0) 40,40,50
   40 CDOST = T(I,1)
      GO TO 20
   50 N = 0.1 * Z + 1.0
      X = 10 * N
      X = (X-Z) * 0.1
      CDOST = X * T(I,N-1) + (1.0-X) * T(I,N)
      GO TO 20
      END
