* $Id: rout4.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT4
C
#include "gcalor/cbert.inc"
C
      CALL CALGEO
      IF(I1) 20,10,10
   10 CURR(3)=PNMS
C     PI+ OR -MASS/CM
      CURR(1)=NO
      CALL PARTIN
      CALL SPAC32(32)
   20 RETURN
      END
