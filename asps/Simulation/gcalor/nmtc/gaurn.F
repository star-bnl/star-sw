* $Id: gaurn.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION GAURN(X)
C
#include "geant321/crandm.inc"
C
   10 Y = EXPRNF(X1)
      Z = EXPRNF(X2)
      TEST = (Y-1.0)**2/2.
      IF(TEST-Z) 20,20,10
   20 R1 = 2.0 * RANDC(ISEED) - 1.0
      IF(R1) 30,40,40
   30 Y = -Y
   40 GAURN = Y
      RETURN
      END
