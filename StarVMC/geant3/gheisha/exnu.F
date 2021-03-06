*
* $Id: exnu.F,v 1.1.1.2 2009/02/01 17:10:27 fisyak Exp $
*
* $Log: exnu.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:27  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:06  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:37  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:39  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:18  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:58  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.38  by  S.Giani
*-- Author :
      FUNCTION EXNU(EK1)
C
C *** NUCLEAR EVAPORATION AS FUNCTION OF ATOMIC NUMBER ATNO ***
C *** AND KINETIC ENERGY EKIN OF PRIMARY PARTICLE ***
C *** NVE 04-MAR-1988 CERN GENEVA ***
C
C ORIGIN : H.FESEFELDT (10-DEC-1986)
C
C
#include "geant321/s_defcom.inc"
      DIMENSION RNDM(2)
C
      EXNU=0.
      IF(ATNO2.LT.1.5) GO TO 9999
      MAGIC=0
      IF(INT(ZNO2+0.1).EQ.82) MAGIC=1
      EKIN1=EK1
      IF(EKIN1.LT.0.1) EKIN1=0.1
      IF(EKIN1.GT.4.) EKIN1=4.
C**   0.35 VALUE AT 1 GEV
C**   0.05 VALUE AT 0.1 GEV
      CFA=(0.35-0.05)/2.3
      CFA= 0.35+CFA*LOG(EKIN1)
      IF(CFA.LT.0.15) CFA=0.15
      EXNU=7.716*CFA*EXP(-CFA)
      ATNO3=ATNO2
      IF(ATNO3.GT.120.) ATNO3=120.
      CFA=((ATNO3-1.)/120.)*EXP(-(ATNO3-1.)/120.)
      EXNU=EXNU*CFA
      FPDIV=1.-0.25*EKIN1**2
      IF(FPDIV.LT.0.50) FPDIV=0.50
      GFA=2.0*((ATNO2-1.)/70.)*EXP(-(ATNO2-1.)/70.)
      ENP(1)=EXNU*FPDIV
      ENP(3)=EXNU-ENP(1)
    4 CALL NORMAL(RAN1)
      CALL NORMAL(RAN2)
      IF(MAGIC.EQ.1) THEN
         RAN1=0.
         RAN2=0.
      END IF
      ENP(1)=ENP(1)*(1.+RAN1*GFA)
      IF(ENP(1).LT.0.) ENP(1)=0.
      ENP(3)=ENP(3)*(1.+RAN2*GFA)
      IF(ENP(3).LT.0.) ENP(3)=0.
    5 EXNU=ENP(1)+ENP(3)
      IF(EXNU.LT.EK1) GOTO 10
      CALL GRNDM(RNDM,2)
      ENP(1)=ENP(1)*(1.-0.5*RNDM(1))
      ENP(3)=ENP(3)*(1.-0.5*RNDM(2))
      GOTO 5
 10   CONTINUE
      IF (NPRT(4))
     $ WRITE(NEWBCD,1001) XEND,YEND,ZEND,EXNU,ENP(1),ENP(3)
 1001 FORMAT(' *EXNU* NUCLEAR EXCITATION AT X,Y,Z = ',3(G12.5,1X)/
     $ 1H ,7X,'EXNU,ENP(1),ENP(3) = ',3(G12.5,1X))
C
 9999 CONTINUE
      END
