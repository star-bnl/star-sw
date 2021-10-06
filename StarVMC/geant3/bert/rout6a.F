* $Id: rout6a.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT6A
      SAVE
C
#include "gcalor/cbert.inc"
C
   10 I1=0
      CALL SPISOM
      STRKP=-2.0
      I1=1
      CALL SPISOM
      STRKP=-1.0
      COM=(AWD(MED)-7.0)*2.0*RCPMV
      IF(COM-E(2))10 ,10 ,20
   20 PM(2)=2.0*DNCMS
      PM(3)=DNCMS
      E(2)=PM(2)+E(2)
      RETURN
      END
