* $Id: cabg6b.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CABG6B
#include "gcalor/cbert.inc"
C
      SAVE
      CALL CZERO
      J=I2
      DO10 I=2,I4
         CE(I)=SPACE(J)
   10 J=J+1
      J=I4+1
      DO20 I=J,6
         CE(I)=S(I3)
   20 I3=I3+1
      RETURN
      END
