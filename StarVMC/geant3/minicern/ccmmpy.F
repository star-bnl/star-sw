*
* $Id: ccmmpy.F,v 1.1 2009/02/25 00:08:24 fisyak Exp $
*
* $Log: ccmmpy.F,v $
* Revision 1.1  2009/02/25 00:08:24  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:09  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:27:59  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:58  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          CCMMPY(M,N,X,X12,X21,Y,Y2,Z,Z2)
          COMPLEX             X(*),X12(*),X21(*),Y(*),Y2(*),Z(*),Z2(*)
          COMPLEX             A, B
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX             SUM, ZERO, F
          F(A,B,SUM)  =  CONJG(A)*B + SUM
          DATA      ZERO      /  (0.,0.)  /
#endif
#if (defined(CERNLIB_NUMCDBLE))&&(!defined(CERNLIB_NUMND))
          COMPLEX*16          SUM, ZERO, DC, F
          DC(A)       =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          F(A,B,SUM)  =  DCONJG(DC(A))*DC(B) + SUM
          DATA      ZERO      /  (0.D0,0.D0)  /
#endif
#if (defined(CERNLIB_NUMCDBLE))&&(defined(CERNLIB_NUMND))
          COMPLEX*16          SUM, ZERO, DC, F
          DC(A)       =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          F(A,B,SUM)  =  CDCONJG(DC(A))*DC(B) + SUM
          DATA      ZERO      /  (0.D0,0.D0)  /
#endif
          IF(M .LE. 0  .OR.  N .LE. 0)  RETURN
#include "cxij.inc"
#include "cyj.inc"
#include "czi.inc"
#include "mmpy.inc"
