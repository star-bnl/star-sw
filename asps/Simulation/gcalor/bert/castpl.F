* $Id: castpl.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CASTPL(W)
      REAL*8 W(12)
#include "gcalor/cbert.inc"
      SAVE
C
      DO10 K=2,950,12
         IF(PLVC(K))10,40,10
   10 CONTINUE
   20 I1=1
   30 RETURN
   40 I=K
      J=I+11
C     PLVC(1)=NO. OF TIMES ENTERED FOR STORAGE OF VELOCITY
C     LESS THAN CRITERION
      L=1
      DO50 K=I,J
         PLVC(K)=W(L)
   50 L=L+1
      PLVC(1)=PLVC(1)+1.0
      GOTO30
      END
