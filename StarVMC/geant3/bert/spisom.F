* $Id: spisom.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE SPISOM
#include "gcalor/cbert.inc"
      SAVE
C
   10 CALL CAISOM
      IF(I1)20,20,30
   20 SPACE(176)=PXYZ(2)
      SPACE(177)=PXYZ(6)
      SPACE(178)=PXYZ(10)
      IF(I1)50,40,40
   30 PXYZ(2)=PXYZ(2)+SPACE(176)
      PXYZ(6)=PXYZ(6)+SPACE(177)
      PXYZ(10)=PXYZ(10)+SPACE(178)
      E(2)=(PXYZ(10)*PXYZ(10)+PXYZ(6)*PXYZ(6)+PXYZ(2)*PXYZ(2))/
     11.9032D14
   40 RETURN
   50 I1=1
      GOTO10
      END
