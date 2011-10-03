* $Id: signex.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE SIGNEX
#include "gcalor/cbert.inc"
      SAVE
C
      CALL EXPRN(UNIV)
      EX=EX+UNIV/SIGN
      RETURN
C     EX=DISTANCE IN SAMPLING ROUTINE
C     EXPONENTIAL RANDOM DIVIDED BY SIGMA CI REGION I
      END
