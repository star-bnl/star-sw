* $Id: exprn.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE EXPRN(EXPA)
C
#include "geant321/crandm.inc"
C
      REAL * 8 EXPA,EXPB,WHOLE,EXPAO
C
      WHOLE=0.0
   10 EXPA = RANDC(ISEED)
      EXPAO=EXPA
   20 EXPB = RANDC(ISEED)
      IF(EXPB.LT.EXPA) GO TO 40
C     RANDOM2 IS.GTE.TO RANDOM1
   30 EXPA=EXPAO+WHOLE
      RETURN
   40 EXPA = RANDC(ISEED)
      IF(EXPA.LT.EXPB) GO TO 20
      WHOLE=WHOLE+1.0
      GO TO 10
      END
