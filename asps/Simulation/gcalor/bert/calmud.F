* $Id: calmud.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CALMUD(SINE,INP)
C
#include "geant321/crandm.inc"
C
      REAL * 8 SINE
C
      SINE= RANDC(ISEED)
      SINE = 5.0 D1 * SINE
      INP = IDINT(SINE + 0.1D1)
      SINE=DFLOAT(INP)-SINE
C     SINE=(.02N-R)/.02=N-R/.02   N=INPT   R/.02=(N-1)+X
      RETURN
      END
