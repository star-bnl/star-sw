* $Id: cbover.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CBOVER(V,VE,VER)
      SAVE
C
#include "gcalor/cbert.inc"
      REAL *8 V,VE,VER
C
      VER=DSQRT(1.0-((VE*VE)/((V*RCPMV+VE)**2)))
      VER=(DSQRT(VER*(6.91D-1+(VER*11.09D-1))+1.08D-1))/VER
      RETURN
      END
