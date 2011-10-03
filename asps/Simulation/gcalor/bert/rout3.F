* $Id: rout3.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT3
C
#include "gcalor/cbert.inc"
C
      IF(NO-4)10 ,20 ,20
C     BG6A IN ORIGINAL
   10 ISW(11)=1
      GOTO30
   20 ISW(11)=0
   30 CALL UNDIS
      INC=1
      RETURN
      END
