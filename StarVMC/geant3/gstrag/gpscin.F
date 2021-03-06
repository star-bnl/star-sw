*
* $Id: gpscin.F,v 1.1.1.2 2009/02/01 17:10:33 fisyak Exp $
*
* $Log: gpscin.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:33  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:50  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:56  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:41  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:21  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:38  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.25  by  S.Giani
*-- Author :
      FUNCTION G3PSCIN(EBEG,EEND,ECUR,COEFF)
C.
C.    ******************************************************************
C.    *                                                                *
C.    ******************************************************************
C.
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
      REAL COEFF(4)
      PARAMETER (ONE=1)
C.
C.    ------------------------------------------------------------------
C.
      YBEG = EBEG/ECUR
      YEND = EEND/ECUR
      YBEGIN = ONE/YBEG
      YENDIN = ONE/YEND
*
      EINV = ONE/ECUR
      C1 = COEFF(1)*EINV
      C2 = COEFF(2)*EINV**2
      C3 = COEFF(3)*EINV**3
      C4 = COEFF(4)*EINV**4
*
      CONS1 = 0.5*(C1+C3)
      CONS2 = C2+C4
      CONS3 = 0.5*CONS2
      CONS4 = 0.5*C3
      CONS5 = C4/3
*
*     VALBEG = CONS1*F1(YBEG)+CONS3*F2(YBEG)+
*    +         YBEGIN*(CONS2+YBEGIN*(CONS4+YBEGIN*CONS5))
*     VALEND = CONS1*F1(YEND)+CONS3*F2(YEND)+
*    +         YENDIN*(CONS2+YENDIN*(CONS4+YENDIN*CONS5))
*
*     G3PSCIN = VALEND-VALBEG
      G3PSCIN =
     +      CONS1*LOG(ABS((YEND**2-1)*YBEG**2/((YBEG**2-1)*YEND**2)))+
     +      CONS2*(YENDIN-YBEGIN)+
     +      CONS3*LOG(ABS((1-YEND)*(1+YBEG)/((1-YBEG)*(1+YEND))))+
     +      CONS4*(YENDIN**2-YBEGIN**2)+CONS5*(YENDIN**3-YBEGIN**3)
*
      END
