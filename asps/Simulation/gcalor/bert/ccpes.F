* $Id: ccpes.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CCPES
      SAVE
#include "gcalor/cbert.inc"
C
#include "gcalor/crn.inc"
C
      I1=0
      I = IDINT(CURR(1) + 5.0D-2)
      COUNT(I) = COUNT(I) + 1.0D0
C*** COUNT NO. OF TIMES EACH TYPE OF PARTICLE ESCAPES
      IF(CURR(1)-2.0)10,20,10
   10 K=CURR(10)+9.05
      GOTO30
   20 K=CURR(10)+3.05
   30 IF(ESPS(1)-60.0)60,40,40
   40 I1=1
C     STORAGE ALREADY FILLED
   50 RETURN
   60 L=ESPS(1)*8.D0 + 2.05D0
      ESPS(L)=CURR(1)
      ESPS(L+1)=CURR(2)-SPACE(K)
      M=13.05-CURR(11)
      CC(M)=CC(M)+1.0
      M=4
      L=L+2
      N=L+2
      DO70 I=L,N
         ESPS(I)=CURR(M+3)
         ESPS(I+3)=CURR(M)
   70 M=M+1
      ESPS(1)=ESPS(1)+1.0
      GOTO50
      END
