* $Id: calnnn.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CALNNN
#include "gcalor/cbert.inc"
C
      FMAX(1) = 0.46 D-24
      FMAX(2) = 1.4 D-24
      DO10 I=3,6
   10 FMAX(I)=0.0
      RETURN
      END
