* $Id: castph.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CASTPH(W)
      REAL*8 W(11)
#include "gcalor/cbert.inc"
      SAVE
C
      IF(PGVC(1)-440.0)10,40,40
   10 I=PGVC(1)+2.005
      J=I+10
C     PGVC(1)=NO.OF TIMES VELOCITY GREATER THAN CRITERION ENTERED
      L=1
      DO20 K=I,J
         PGVC(K)=W(L)
   20 L=L+1
      PGVC(1)=PGVC(1)+11.0
   30 RETURN
   40 I1=1
      GOTO30
      END
