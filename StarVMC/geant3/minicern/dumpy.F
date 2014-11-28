*
* $Id: dumpy.F,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dumpy.F,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:14  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:04  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:49:00  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DUMPY(N,U,U12,U22,Y,Y2,Z,Z2)
          DOUBLE PRECISION    U(*),U12(*),U22(*),Y(*),Y2(*),Z(*),Z2(*)
          DOUBLE PRECISION    A, B, SUM, ZERO, F
          F(A,B,SUM)  =  A*B + SUM
          DATA ZERO    / 0.D0 /
          IF(N .LE. 0)  RETURN
#include "dujk.inc"
#include "dyj.inc"
#include "dzi.inc"
#include "umpy.inc"
