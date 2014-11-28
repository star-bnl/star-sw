*
* $Id: dmbil.F,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: dmbil.F,v $
* Revision 1.1  2009/02/25 00:08:25  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:13  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:02  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          DOUBLE PRECISION FUNCTION DMBIL(N,X,X2,Y,Y12,Y21,Z,Z2)
          DOUBLE PRECISION X(*),X2(*),Y(*),Y12(*),Y21(*),Z(*),Z2(*)
          DOUBLE PRECISION A, B, SUM, ZERO, F, G, SXYZ, SYZ
          F(A,B,SUM)  =  A*B + SUM
          G(A,B,SUM)  =  A*B + SUM
          DATA      ZERO      /  0.D0  /
          SXYZ  =  ZERO
          IF(N .LE. 0)  GOTO 20
#include "dxi.inc"
#include "dyij.inc"
#include "dzj.inc"
#include "mbil.inc"
  20      DMBIL  =  SXYZ
          RETURN
          END
