*
* $Id: gdeca2.F,v 1.1.1.2 2009/02/01 17:10:31 fisyak Exp $
*
* $Log: gdeca2.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:31  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:35  fisyak
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
* Revision 1.1.1.1  2002/06/16 15:18:40  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:20  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:23  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE G3DECA2(XM0,XM1,XM2,PCM)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Simulates two body decay process with isotropic angular       *
C.    *  distribution in CMS.                                          *
C.    *                                                                *
C.    *    ==>Called by : G3DECAY                                      *
C.    *       Author    G.Patrick  *********                           *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gconsp.inc"
      DIMENSION PCM(4,2)
      DIMENSION RNDM(2)
C.
C.    ------------------------------------------------------------------
C.
C             Generate first decay product in CMS.
C
      E1=(XM0*XM0+XM1*XM1-XM2*XM2)/(2.*XM0)
      P1=SQRT(ABS((E1-XM1)*(E1+XM1)))
C
C             Isotropic decay angular distribution.
C
      CALL GRNDM(RNDM,2)
      COSTH=2.*RNDM(1)-1.
      IF(ABS(COSTH).GE.1.0) THEN
         COSTH=SIGN(1.,COSTH)
         SINTH=0.
      ELSE
         SINTH=SQRT((1.-COSTH)*(1.+COSTH))
      ENDIF
      PHI=TWOPI*RNDM(2)
C
C             Polar co-ordinates to momentum components.
C
      PCM(1,1)=P1*SINTH*COS(PHI)
      PCM(2,1)=P1*SINTH*SIN(PHI)
      PCM(3,1)=P1*COSTH
      PCM(4,1)=E1
C
C             Generate second decay product.
C
      PCM(1,2)=-PCM(1,1)
      PCM(2,2)=-PCM(2,1)
      PCM(3,2)=-PCM(3,1)
      PCM(4,2)=SQRT(PCM(1,2)**2+PCM(2,2)**2+PCM(3,2)**2+XM2*XM2)
C
      END
