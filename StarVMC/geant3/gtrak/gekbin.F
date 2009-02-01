*
* $Id: gekbin.F,v 1.1.1.2 2009/02/01 17:10:33 fisyak Exp $
*
* $Log: gekbin.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:33  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:51  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:56  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:41  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:21  fca
* AliRoot sources
*
* Revision 1.2  1998/01/21 16:44:37  gunter
* Add protection of iekbin getting too big due to numerical inacurecies
*
* Revision 1.1.1.1  1995/10/24 10:21:40  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.23  by  S.Giani
*-- Author :
      SUBROUTINE G3EKBIN
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to find bin number in kinetic energy table       *
C.    *       stored in ELOW(NEKBIN)                                   *
C.    *                                                                *
C.    *  ==>Called by : G3BREME,G3BREMM,G3COMP,G3DRAY,G3HEISH,G3PAIRM, *
C.    *                   G3LTRAC,G3TELEC,G3THADR,G3TMUON              *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gctrak.inc"
#include "geant321/gcmulo.inc"
#include "geant321/gckine.inc"
#include "geant321/gconsp.inc"
C.
C.    ------------------------------------------------------------------
C.
      EK=GEKIN
      IF (ITRTYP.EQ.4.OR.ITRTYP.EQ.8) EK=GEKIN*PMASS/AMASS
      IF (EK.LT.ELOW(1)) THEN
         IEKBIN=1
         GEKRAT=0.
      ELSEIF (EK.GE.ELOW(NEK1)) THEN
         IEKBIN=NEKBIN
         GEKRAT=1.
      ELSE
         IEKBIN=GEKA*LOG10(EK) + GEKB
         IF (IEKBIN.GT.NEKBIN ) IEKBIN=NEKBIN
         GEKRAT=(EK-ELOW(IEKBIN))/(ELOW(IEKBIN+1)-ELOW(IEKBIN))
         GEKRAT=MIN(1.,MAX(GEKRAT,0.))
      ENDIF
C
      END
