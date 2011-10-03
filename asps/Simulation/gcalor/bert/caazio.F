* $Id: caazio.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CAAZIO(SINE,COSINE)
C
#include "geant321/crandm.inc"
      REAL * 8 SINE,COSINE,R1,R2,R1SQ,R2SQ,SUM
C
   10 R1 = RANDC(ISEED)
      R1SQ = R1 * R1
C     XSQ
      R2 = RANDC(ISEED)
      R2SQ = R2 * R2
C     YSQ
      SUM = R1SQ + R2SQ
      IF(SUM.GT.1.0) GO TO 10
      SUM = SUM * 0.5
C     (XSQ+YSQ)/2
      COSINE = (SUM-R1SQ) / SUM
C     (YSQ-XSQ)/(XSQ+YSQ)
      SINE = (R1*R2) / SUM
C     (2*X*Y)/(XSQ+YSQ)
      R1 = RANDC(ISEED)
      IF(R1.LT.0.5) GO TO 20
      SINE = -SINE
   20 RETURN
      END
