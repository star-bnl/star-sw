* $Id: exprnf.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION EXPRNF(A)
C
#include "geant321/crandm.inc"
C
      REAL I
C
      I = 0.0
   10 X = RANDC(ISEED)
      Z = X
   20 Y = RANDC(ISEED)
      IF(Z-Y) 50 ,50 ,30
   30 Z = RANDC(ISEED)
      IF(Z-Y) 20 ,40 ,40
   40 I = I + 1.0
      GO TO 10
   50 EXPRNF = X + I
      RETURN
      END
