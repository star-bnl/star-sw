*
* $Id: rumpy.F,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rumpy.F,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:18  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:18  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:49:00  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RUMPY(N,U,U12,U22,Y,Y2,Z,Z2)
          REAL                U(*),U12(*),U22(*),Y(*),Y2(*),Z(*),Z2(*)
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
          IF(N .LE. 0)  RETURN
#include "rujk.inc"
#include "ryj.inc"
#include "rzi.inc"
#include "umpy.inc"
