*
* $Id: cumna.F,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: cumna.F,v $
* Revision 1.1  2009/02/25 00:08:25  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:12  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:01  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:59  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CUMNA(N,U,U12,U22,Y,Y2,Z,Z2)
          COMPLEX             U(*),U12(*),U22(*),Y(*),Y2(*),Z(*),Z2(*)
          COMPLEX             A, B, SIGNF
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX             SUM, F
          F(A,B,SUM)  =  -A*B + SUM
#endif
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX*16          SUM, DC, F
          DC(A)       =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          F(A,B,SUM)  =  -DC(A)*DC(B) + SUM
#endif
#if defined(CERNLIB_IBMAIX)
          SIGNF(A)    =  A*1
#else
          SIGNF(A)    =  A
#endif
          IF(N .LE. 0)  RETURN
#include "cujk.inc"
#include "cyj.inc"
#include "czi.inc"
#include "umpa.inc"
