*
* $Id: gsaphi.F,v 1.1.1.2 2009/02/01 17:10:26 fisyak Exp $
*
* $Log: gsaphi.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:26  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:55  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:36  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:25  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:39  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:17  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:55  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.30  by  S.Giani
*-- Author :
      SUBROUTINE GSAPHI(V,PHIL,PHIH,SAF)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *    Subroutine GSAPHI computes the 'safety radius' from the     *
C.    *    point V to phi limits PHIL and PHIH. The safety radius is   *
C.    *    returned in SAF.                                            *
C.    *       A.C.McPherson   5th Sept. 1984.                          *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gconsp.inc"
C
      DIMENSION V(3)
C
      R=V(1)**2+V(2)**2
      IF(R.GT.0.0) R=SQRT(R)
C
      PHL=PHIL*DEGRAD
      PHH=PHIH*DEGRAD
C
C              Check for case of complete 360 degree region.
C
      SAF=BIG
      DPHI=PHIH-PHIL
      IF(DPHI.LE.0.0) DPHI=DPHI+360.0
      IF(DPHI.GE.360.0) GO TO 10
C
      SAF=R
      SL=SIN(PHL)
      CL=COS(PHH)
      IF(SL*V(2)+CL*V(1).GT.0.0) SAF=ABS(SL*V(1)-CL*V(2))
C
      SAF2=R
      SH=SIN(PHH)
      CH=COS(PHH)
      IF(SH*V(2)+CH*V(1).GT.0.0) SAF2=ABS(SH*V(1)-CH*V(2))
C
      IF(SAF2.LT.SAF) SAF=SAF2
C
   10 CONTINUE
      RETURN
      END
