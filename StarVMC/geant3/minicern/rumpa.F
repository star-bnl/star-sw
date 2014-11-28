*
* $Id: rumpa.F,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rumpa.F,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:18  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:18  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RUMPA(N,U,U12,U22,Y,Y2,Z,Z2)
          REAL                U(*),U12(*),U22(*),Y(*),Y2(*),Z(*),Z2(*)
          REAL                A, B, SIGNF
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    SUM, F
          F(A,B,SUM)  =  DBLE(A)*DBLE(B) + SUM
#endif
#if !defined(CERNLIB_NUMRDBLE)
          REAL                SUM, F
          F(A,B,SUM)  =  A*B + SUM
#endif
          SIGNF(A)  =  A
          IF(N .LE. 0)  RETURN
#include "rujk.inc"
#include "ryj.inc"
#include "rzi.inc"
#include "umpa.inc"
