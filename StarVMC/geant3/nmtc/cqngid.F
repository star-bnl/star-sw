* $Id: cqngid.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CQNGID
      IMPLICIT REAL *8  (A-H,O-Z)
C
#include "geant321/crandm.inc"
#include "gcalor/cbert3.inc"
#include "gcalor/cisob2.inc"
      SAVE
C
C     ******************************************************************
C****    CALCULATES  COS AND SIN THETA,SIN AND COS PHI    **************
C     ******************************************************************
      ICURR= CURR(1)
      IT=0
      GO TO(10,10,30,30,30),ICURR
C****  INCIDENT PARTICLE - NUCLEON
   10 IF(IT.EQ.21.OR.IT.EQ.22)GO TO 20
C****  SINGLE  PRODUCTION
      IF(RLKE.GT.3500.0D0) CALL CERROR('CQNGID1$')
      IF(RLKE.LT.500.0D0)GO TO 70
      TESISO= 0.75D0
      IF(RLKE.LT.1000.0D0)GO TO 50
      TESISO= 0.5D0
      IF(RLKE.LT.1300.0D0)GO TO 50
      TESISO= 0.25D0
      IF(RLKE.LT.2500.0D0)GO TO 50
      GO TO 60
C****  DOUBLE PRODUCTION
   20 IF(RLKE.GT.3500.0D0) CALL CERROR('CQNGID2$')
      GO TO 60
C**** INCIDENT PARTICLE-PION
   30 R = RANDC(ISEED)
      IF(RLKE.GT.2500.0D0) CALL CERROR('CQNGID3$')
      CST= -0.9999995D0
      SNT=  0.003162D0
      IF(IT.NE.11)GO TO 40
      IF(R.LE.0.75D0)GO TO 70
      GO TO 80
C****  (PI+)-(P),(PI-)-(N)
C****  (PI0)-(N),(PI0)-(P)
   40 IF(IT.NE.12.AND.IT.NE.28) CALL CERROR('CQNGID4$')
      IF(RLKE.LT.500.0D0)CST=-CST
      IF(R.LE.0.80D0)GO TO 70
      GO TO 80
   50 R = RANDC(ISEED)
      IF(R.LE.TESISO)GO TO 70
C**** BACKWARD/FORWARD
   60 R = RANDC(ISEED)
C****  TEST FOR DIRECTION
      CST= 0.9999995D0
      SNT= 0.003162D0
      IF(R.LE.0.5)GO TO 80
      CST= -0.9999995D0
      GO TO 80
C****  ISOTROPIC
   70 CALL CAPOL1(CST,SNT)
C****  CALCULATES  COS,SIN PHI
   80 CALL CAAZIO(SOPC,SOPS)
      RETURN
      END
