* $Id: p1cli.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE P1CLI
#include "gcalor/cbert.inc"
C
      PXYZ(1)=0.0
      PXYZ(5)=0.0
      PXYZ(9)=DSQRT(E(1)*E(1)-PM(1)*PM(1))
      P1OE1=PXYZ(9)/E(1)
      RETURN
C     MOMENTUM X AND Y COORDINATES,PARTICLE 1 =0.0
C     Z COORD. =TOTAL ENERGY SQUARED-MASS SQ. TO THE 1/2
C     FOR PARTICLE ONE.  P1OE1=CURRENT(MOMENT/TOTAL)
      END
