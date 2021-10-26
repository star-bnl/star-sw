* $Id: fltrnf.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
C*********************************************************************
      FUNCTION FLTRNF(X)
C*********************************************************************
#include "geant321/crandm.inc"
      FLTRNF = RANDC(ISEED)
      RETURN
      END
