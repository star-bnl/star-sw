* $Id: rout16.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT16(T)
      REAL*8 T(6426)
C
#include "gcalor/cbert.inc"
C
      CALL CALMUD(SNT,INPT)
      CALL CRDET(51,T(1),RLKE)
      I2 = 0
      CST = CRDT(2) - DABS(SNT*(CRDT(2) - CRDT(1)))
      GO TO (10 ,30 ,40 ), I3
   10 I3=-1
   20 RETURN
   30 I3=0
      GOTO20
   40 I3=1
      GOTO20
      END
