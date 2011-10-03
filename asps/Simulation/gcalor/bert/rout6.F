* $Id: rout6.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT6
C
#include "geant321/crandm.inc"
C
#include "gcalor/cbert.inc"
C
      IF(I3)10 ,90 ,90
   10 XABS=1.0
      MED=CLSM
      KNOT=NOT
      VALUE1 = RANDC(ISEED)
      IF(ISW(11))50 ,20 ,50
   20 IF(VALUE1-PPMDA)60 ,40 ,40
C     PROB. PIM-DEUT ABS.
   30 RETURN
   40 I3=1
      GO TO 30
   50 IF(VALUE1-PPPDA)60 ,40 ,40
C     PROB. PIP-DEUT ABS.
   60 IF(ISW(11))80 ,70 ,80
   70 IT=13
      ABSEC=PMAC(MED)
      GO TO 90
   80 IT=14
      ABSEC=-HVN(MED)
   90 STRKP=-1.0
      I1=0
      I2=MED
      CALL CBBBBB
      I3=0
      GO TO 30
      END
