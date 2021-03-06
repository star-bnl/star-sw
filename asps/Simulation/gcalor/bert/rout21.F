* $Id: rout21.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT21(V,W,X,Y,Z)
C
#include "geant321/crandm.inc"
#include "gcalor/cbert.inc"
C
      REAL*8 V(161),W(101),X(161),Y(130),Z(176)
      SAVE
C
      VALUE2=RLKE*4.81633308D24+9.0554256D27
C     E(TR)**2=RLKE*RCPMV*2*NCMS+4*NCMS**2
      VALUE3=DSQRT(VALUE2)
      GO TO (10  ,100 ,110 ,240 ),I3
   10 ISW(12)=0
   20 PT(38)=0.0
      I1=0
      ANS=RLKE
   30 VALUE1=ANS-300.0
      CALL CRDET(1,V(1),VALUE1)
C     (NUC-NUC) F(TR) ISOBAR SAMPLING
      FTR=CRDT(1)
   40 SN = RANDC(ISEED)
      COM=SN*FTR
C     R PRIME=F(TR)*RANDOM
      CALL CAGENE(W(1))
C     (NUC-NUC)MASS OF ISOBAR S.P.    M(R PRIME)
      IF(I1)130 ,50  ,140
   50 COM1=(COM*COM-SQNM+VALUE2)/(2.0*VALUE3)
C     E GAMMA
      A=COM1*COM1-COM*COM
      IF(A)60  ,70  ,70
   60 PGCNT=PGCNT+1.0
      GOTO40
C
CZ changed in order to keep exponent small 5/21/92
   70 UNIVER=DSQRT(A)*COM1*(1.0D0-COM1/VALUE3)
CZ end of change
CZ
C     F(M,TR)=P GAMMA*E GAMMA*E DELTA/E
      CALL CRDET(1,X(1),VALUE1)
C     (NUC-NUC)FMAX(TR) ISOBAR SAMPLING S.P.
      COM1 = RANDC(ISEED)
      IF(COM1-(UNIVER/CRDT(1)))80  ,80  ,40
   80 PM(4)=DNCMS
      PM(3)=COM
      CALL CANGID
      PT(4)=DNCMS
      PT(28)=DNCMS
      CALL CALP19
   90 RETURN
  100 ISW(12)=2
      GOTO20
  110 ISW(13)=0
  120 I1=-1
      ANS=((VALUE3-PNMS)**2-9.0554256D27)/4.81633308D24
      GO TO 30
C     TR PRIME     COM1=RLKE PRIME
  130 COM1=((VALUE3+DNCMS-COM)**2-9.0554256D27)/4.81633308D24
      COM2=COM
      ANS=COM1
      COM4=FTR
      I1=1
      GO TO 30
  140 COM1=(COM2*COM2-COM*COM+VALUE2)/(2.0*VALUE3)
C     E EPSILON
      A=COM1*COM1-COM2*COM2
      IF(A)150 ,160 ,160
  150 PECNT=PECNT+1.0
      GOTO170
C     F(M1,M2,TR)=P EPSILON*E EPSILON*E ZETA/E
C
CZ changed in order to keep exponent small 5/21/92
  160 UNIVER=DSQRT(A)*COM1*(1.0D0-COM1/VALUE3)
CZ end of change
CZ
      VALUE1=RLKE-920.0
      CALLCRDET(1,Y(1),VALUE1)
C     (NUC-NUC)FMAX(TR) ISOBAR SAMPLING D.P.  FMAX(M1,M2,TR)
      SN = RANDC(ISEED)
      IF(SN-(UNIVER*FTR/(CRDT(1)*COM4)))180 ,180 ,170
  170 FTR=COM4
      I1=-1
      GOTO40
  180 VALUE1 = RANDC(ISEED)
      IF(VALUE1-.5)190 ,190 ,200
  190 PM(3)=COM2
      PM(4)=COM
      GOTO210
  200 PM(3)=COM
      PM(4)=COM2
  210 CALLCANGID
      PT(16)=DNCMS
      PT(40)=DNCMS
      IF(ISW(13))220 ,230 ,220
  220 CALL CRDET(1,Z(1),RLKE)
      VALUE1=CRDT(1)
C     (N-P)FRACT.FIN.STA.3/2 L.E.
  230 PT(2)=3.0
      PT(4)=POMS
      PT(14)=1.0
      PT(26)=3.0
      PT(28)=POMS
      PT(38)=1.0
      CALL CALP28
      GO TO 90
  240 ISW(13)=2
      GO TO 120
      END
