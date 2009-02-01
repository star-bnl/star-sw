*
* $Id: coranh.F,v 1.1.1.2 2009/02/01 17:10:27 fisyak Exp $
*
* $Log: coranh.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:27  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:05  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:37  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.2  2002/07/10 09:45:00  morsch
* Gheisha corrections suggested by Gary Bower (FNAL).
*
* Revision 1.1.1.1  1995/10/24 10:21:01  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.39  by  S.Giani
*-- Author :
      SUBROUTINE CORANH(NIHIL,NFL)
C
C *** NUCLEAR INTERACTIONS FOR HEAVY FRAGMENTS ***
C *** NVE 06-MAY-1988 CERN GENEVA ***
C
C ORIGIN : H.FESEFELDT (09-JULY-1987)
C
#include "geant321/s_defcom.inc"
C
      NIHIL=0
      IF(AMAS.GT.0.)   GO TO 9999
      IF(IPART.LT.14)  GO TO 9999
      IF(IPA(1).GE.14) GO TO 9999
      IF(IPA(2).GE.14) GO TO 9999
      NIHIL=1
      RETURN
C**
C**  DO NOT BE CONFUSED, THIS HAS NOTHING TO DO WITH RELATIVISTIC
C**  KINEMATIC
C
      TARMAS=RMASS(14)
      IF (NFL .EQ. 2) TARMAS=RMASS(16)
      EKCOR=1.
      IF(EK.GT.1.) EKCOR=1./EK
      EK=2.*TARMAS+EK*(1.+EKCOR/ATNO2)
      EN=EK+ABS(AMAS)
      P =SQRT(ABS(EN*EN-AMAS*AMAS))
      S =AMAS*AMAS+TARMAS**2+2.0*TARMAS*EN
      RS=SQRT(S)
      ENP(5)=EK
      ENP(6)=EN
      ENP(7)=P
      ENP(8)=S
      ENP(9)=RS
C**
C**  EVAPORATION
C**
      TKIN=EXNU(EK)
      ENP(5)=EK-TKIN
      IF(ENP(5).LT.0.0001) ENP(5)=0.0001
      ENP(6)=ENP(5)+ABS(AMAS)
      ENP(7)=ENP(6)*ENP(6)-AMASQ
      ENP(7)=SQRT(ABS(ENP(7)))
      ENP(8)=AMASQ+RMASS(14)**2+2.*RMASS(14)*ENP(6)
      ENP(9)=SQRT(ENP(8))
C**  CHECK AVAILABLE ENERGY FOR FIRST INTERACTION
      IF(ENP(5).GT.CENG(3)) GO TO 9999
      ENP(5)=0.
      ENP(6)=ABS(AMAS)
      ENP(7)=0.
      ENP(8)=4.*RMASS(14)**2
      ENP(9)=2.*RMASS(14)
C
 9999 CONTINUE
      END
