*
* $Id: gdrsga.F,v 1.1.1.2 2009/02/01 17:10:31 fisyak Exp $
*
* $Log: gdrsga.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:31  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:36  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:54  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:25  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:41  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:20  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:24  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE G3DRSGA
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Calculates cross-section in current material                  *
C.    *  for Delta rays (Moller or Bhabba)                             *
C.    *                                                                *
C.    *    ==>Called by : G3PHYSI                                       *
C.    *       Authors    D.Ward, L.Urban  *********                    *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gctrak.inc"
#include "geant321/gconsp.inc"
#include "geant321/gcmate.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gccuts.inc"
#include "geant321/gcmulo.inc"
C.
C.    ------------------------------------------------------------------
C.
      Q(JDRAY+IEKBIN       )=BIG
      Q(JDRAY+IEKBIN+  NEK1)=BIG
      Q(JDRAY+IEKBIN+2*NEK1)=BIG
      IF(Z.LT.1.) GOTO 99
      JPROB=LQ(JMA-4)
      TKIN=ELOW(IEKBIN)
C
C=======>              A,   MUONS
C
      IF(TKIN.GT.DCUTM)THEN
         XE=TKIN+EMMU
         GAM=XE/EMMU
         GAM2=GAM**2
         BET2=1.-1./GAM2
         TMAX=2.*EMASS*(GAM2-1.)/
     +     (1.+2.*GAM*EMASS/EMMU+(EMASS/EMMU)**2)
         IF(TMAX.GT.DCUTM)THEN
            Y=DCUTM/TMAX
            SIG=(1.-Y+BET2*Y*LOG(Y))/DCUTM
            SIG=SIG+(TMAX-DCUTM)/(2.*XE**2)
            SIG=SIG*Q(JPROB+17)*EMASS/BET2
            IF(SIG.GT.0.)Q(JDRAY+IEKBIN+2*NEK1)=1./SIG
         ENDIF
      ENDIF
C
C======>            B,    ELECTRONS
C
      IF(TKIN.GT.DCUTE)THEN
C
         XE=TKIN+EMASS
         GAM=XE/EMASS
         GAM2=GAM*GAM
         BET2=1.-1./GAM2
         T=GAM-1.
         X=DCUTE/(T*EMASS)
         X1=1.-X
C
C     Moller
C
         IF(T.GT.2.*DCUTE/EMASS)THEN
            B1=T*T/GAM2
            B2=(2.*GAM-1.)/GAM2
            SIG=(B1*(0.5-X)+1./X-1./X1-B2*LOG(X1/X))/BET2
            SIG=Q(JPROB+17)*SIG/T
            IF(SIG.GT.0.)Q(JDRAY+IEKBIN)=1./SIG
         ENDIF
C
C     Bhabha
C
         IF(T.GT.DCUTE/EMASS)THEN
            Y=1./(GAM+1.)
            Y2=Y*Y
            C=1.-2.*Y
            B1=2.-Y2
            B2=C*(3.+Y2)
            C2=C*C
            B4=C2*C
            B3=C2+B4
            SIG=(1./X-1.)/BET2+B1*LOG(X)
     +           +B2*(1.-X)-B3*(1.-X*X)/2.+B4*(1.-X**3)/3.
            SIG=Q(JPROB+17)*SIG/T
            IF(SIG.GT.0.)Q(JDRAY+IEKBIN+NEK1)=1./SIG
         ENDIF
      ENDIF
C
  99  RETURN
      END
