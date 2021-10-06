* $Id: capol1.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CAPOL1(CS,SI)
C
#include "geant321/crandm.inc"
C
      REAL*8 CS, SI
C
      CS = RANDC(ISEED)
      S = 2.0 * RANDC(ISEED) - 1.0
      IF(S.LT.0) CS = -CS
      SI = DSQRT(1.0-(CS*CS))
      RETURN
      END
