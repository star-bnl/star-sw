* $Id: castpr.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CASTPR
#include "gcalor/cbert.inc"
C
      SAVE
      I1=0
      MED=CLSM
      DO90 I=3,39,12
         K=I
         IF(PT(I))10,90,10
   10    IF(PT(K-1)-2.0)20,30,20
   20    PT(K-2)=PT(K)-SPACE(MED+9)
         GOTO60
   30    PT(K-2)=PT(K)-SPACE(MED+3)
   40    IF(PT(K-2)-500.0)50,50,80
   50    CALLCASTPL(PT(K-2))
C     VELOCITY LESS THAN CRITERION
         IF(I1)100,90,100
   60    IF(PT(K-1)-1.0)70,40,70
   70    PT(K-2)=(DNCMS*PT(K-2))/PT(K+1)
         GOTO40
   80    CALLCASTPH(PT(K-1))
C     VELOCITY GREATER THAN CRITERION
         IF(I1)100,90,100
   90 CONTINUE
  100 RETURN
      END
