* $Id: gtiso.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE GTISO(U,V,W)
C
#include "geant321/crandm.inc"
C
   10 Z = RANDC(ISEED)
      X = 0.687368 * SFLRAF(X)
      Y = 0.687368 * SFLRAF(X)
      XSQ = X * X
      YSQ = Y * Y
      ZSQ = Z * Z
      D = XSQ + YSQ + ZSQ
      IF(D*D-Z) 20 ,20 ,10
   20 U = 2.0*X*Z/D
      V = 2.0*Y*Z/D
      W = (ZSQ-XSQ-YSQ)/D
      RETURN
      END
