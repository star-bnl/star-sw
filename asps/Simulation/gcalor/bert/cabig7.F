* $Id: cabig7.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CABIG7(C,GREAT,IX)
C
#include "geant321/crandm.inc"
C
      REAL*8 C, GREAT
C
      GREAT = C
      I = IX + 1
      DO 10 K = 1,I
         C = RANDC(ISEED)
         IF(C.LT.GREAT) GO TO 10
         GREAT = C
   10 CONTINUE
C     GREAT IS THE LARGEST OF I RANDOM NOS.
      RETURN
      END
