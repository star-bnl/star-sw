* $Id: rout11.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT11(T)
      SAVE
C
#include "gcalor/cbert.inc"
C
      REAL *8 T(6426)
      GO TO (10 ,30 ,40 ,20  ),I3
   10 PT(2)=3.0
   20 IK=IT
      PT(14)=1.0
   30 PM(3)=PNMS
   40 IF(340.0-RLKE)50 ,70 ,70
   50 I3=1
   60 RETURN
   70 CALL CALMUD(SNT,INPT)
      IF(IK-3)100,80 ,90
   80 I3=2
      GO TO 60
   90 I3=3
      GO TO 60
  100 CALL CRDET(51,T(1),RLKE)
      I3=4
      GOTO60
      END
