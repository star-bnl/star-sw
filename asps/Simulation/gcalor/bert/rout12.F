* $Id: rout12.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT12
C
#include "gcalor/cbert.inc"
#include "gcalor/crun.inc"
C
      I3=0
      CALL CAAZIO(SOPC,SOPS)
      CALL CACOLL(0)
      IF(COL(15))10 ,30 ,10
   10 I3=-1
   20 RETURN
   30 IF(KE)10 ,50 ,40
   40 COM = (E(4)-DNCMS)/RCPMV
      GO TO 60
   50 CALL COLE4
   60 I1= -1
      VALUE1=COM
      IF(PT(14)-2.0)70 ,20 ,20
   70 I3=1
      GOTO20
      END
