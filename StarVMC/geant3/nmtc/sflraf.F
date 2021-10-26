* $Id: sflraf.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION SFLRAF(X)
C
#include "geant321/crandm.inc"
C
      SFLRAF = 2.0 * RANDC(ISEED)
      TEMP = 1.0 - SFLRAF
      IF(TEMP) 10 ,20 ,20
   10 SFLRAF = TEMP
   20 RETURN
      END
