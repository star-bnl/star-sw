* $Id: azirn.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE AZIRN(SIN,COS)
#include "geant321/crandm.inc"
C       THIS ROUTINE SELECTS THE AZIMUTHAL ANGLE UNIFORMLY IN THETA
   10 R1 = SFLRAF(DUM)
      R1SQ = R1 * R1
      R2 = RANDC(ISEED)
      R2SQ = R2 * R2
      RSQ = R1SQ + R2SQ
      IF(1.0-RSQ) 10 ,20 ,20
   20 SIN = 2.0 * R1 * R2 / RSQ
      COS = (R2SQ-R1SQ) / RSQ
      RETURN
      END
