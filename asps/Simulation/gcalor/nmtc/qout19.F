* $Id: qout19.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE QOUT19
      IMPLICIT REAL *8  (A-H,O-Z)
#include "gcalor/cbert3.inc"
#include "gcalor/cisob2.inc"
      SAVE
C
   10 PT(3)=PT(3)+((PT(11)-PT(12))/RCPMV)
C     COLLISION ALLOWED
      K=3
      GOTO100
   20 I3=-1
   30 RETURN
   40 I2=2
   50 I1=(K/12)+3
   60 CALL CQSTOR
   70 IF(K-15)80  ,90  ,120
   80 K=15
      GOTO40
   90 K=27
      PT(27)=PT(27)+((PNIDK(12)-PT(K+1))/RCPMV)
  100 IF(K-15)40  ,110 ,110
  110 I2=0
      GOTO50
  120 IF(K-27)20 ,130 ,140
  130 IF(PT(39))140 ,140 ,150
  140 I3=0
      GOTO30
  150 I2=1
      K=39
      GOTO50
      END
