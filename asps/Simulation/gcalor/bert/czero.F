* $Id: czero.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CZERO
#include "gcalor/cbert.inc"
C
      DO 10 I = 1,21
   10 CE(I) = 0.0
      RETURN
      END
