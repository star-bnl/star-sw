* $Id: rout10.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT10
C
#include "gcalor/cbert.inc"
C
      I3=0
      IF(EX-D(4))40 ,40 ,10
   10 CALL SPAC32(31)
      GO TO 20
   20 I3=-1
   30 RETURN
   40 CURR(2)=OUT(15)
      WKRPN(1)=OUT(15)
      WKRPN(4)=OUT(18)
C     K.E. FOR PROTONS AND NEUTRONS REGION 1
      GO TO 30
      END
