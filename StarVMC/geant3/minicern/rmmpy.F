*
* $Id: rmmpy.F,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rmmpy.F,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:18  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:17  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:57  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RMMPY(M,N,X,X12,X21,Y,Y2,Z,Z2)
          REAL                X(*),X12(*),X21(*),Y(*),Y2(*),Z(*),Z2(*)
#if defined(CERNLIB_NUMRDBLE)
          REAL                A, B
          DOUBLE PRECISION    SUM, ZERO, F
          F(A,B,SUM)  =  DBLE(A)*DBLE(B) + SUM
          DATA ZERO    / 0.D0 /
#endif
#if !defined(CERNLIB_NUMRDBLE)
          REAL                A, B, SUM, ZERO, F
          F(A,B,SUM)  =  A*B + SUM
          DATA ZERO    / 0. /
#endif
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "rxij.inc"
#include "ryj.inc"
#include "rzi.inc"
#include "mmpy.inc"
