* $Id: undis.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE UNDIS
#include "gcalor/cbert.inc"
#include "geant321/crandm.inc"
#include "gcalor/cmunpu.inc"
#include "gcalor/crn.inc"
#include "gcalor/cxyinc.inc"
      REAL*8 RAN1,RAN2
      SAVE
C
      IF(BEGRU) 110,130,100
   10 BEGRU=1.0
      DO 20 I = 1,4
   20 RANDS(I)=RANDI(I)
   30 RAN1 = RANDC(ISEED)
      RAN2 = RANDC(ISEED)
      RAN1 = 2.D0*RAN1-1.D0
      RAN2 = 2.D0*RAN2-1.D0
      IF(RAN1**2 + RAN2**2 .GE. 1.D0) GO TO 30
      XI(1) = RAN1*OUT(4)
      XI(2) = RAN2*OUT(4)
      X = XI(1)
      Y = XI(2)
      CURR(4)=XI(1)
      CURR(5) = XI(2)
      XI(3)=-OUT(4)
      DCOS(1)=0.0
      DCOS(2)=0.0
      DCOS(3)=1.0
      MED=4
      CURR(6)=XI(3)
      CURR(7)=0.0
      CURR(8)=0.0
      CURR(9)=1.0
      CURR(10)=MED
C     X, Y AND Z COORDINATES, ALSO ALPHA, BETA AND GAMMA
C     DIRECTION COSINES.  MED=4, (NO. OF GEOM)
   40 RETURN
   50 BEGRU = BEGRU + 1.0 D0
   60 BEGRU=BEGRU+1.0
      IF(CASESN -BEGRU)80,70,70
   70 FRAND = RANDC(ISEED)
      GO TO 30
   80 BEGRU=0.0
      DO90 I=1,4
         RANDI(I)=RANDS(I)
   90 ERAND(I)=RANDS(I)
      GOTO40
C     FINAL RANDOM IN ERAND.  RUN COMPLETED
  100 IF(COUNT(6).EQ.0.0) GO TO 130
  110 COUNT(6) = 0.0
  120 IF(BEGRU) 50,10,60
  130 DO 140 I=1,12
         PCC(I) = CC(I)
  140 NIP(I) = IPEC(I)
      DO 150 I=1,5
  150 PPNB(I) = PNBC(I)
      GO TO 120
C*** WHEN BEGRU = 0, MUNPU COMMON IS ZEROED
      END
