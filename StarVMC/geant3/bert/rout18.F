* $Id: rout18.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT18
      SAVE
C
#include "gcalor/cbert.inc"
C
      GOTO(10  ,20  ,80  ,210 ,170 ,190 ),I3
   10 I=3
      COL(15)=1.0
      K=27
      GO TO 30
   20 I=3
      COL(15)=4.0
      K=15
   30 PNIDK(1)=PM(I)
      J=I
      DO40  L=2,4
         PNIDK(L)=PXYZ(J)
   40 J=J+4
      PNIDK(5)=E(I)
      PNIDK(6)=PT(K-11)
      CALLCALIDK
      IF(K-27)60  ,50  ,60
   50 PT(15)=PT(15)+((PNIDK(12)-PNIDK( 6))/RCPMV)
   60 PT(K)=PT(K)+((PNIDK(13)-DNCMS)/RCPMV)
      I3=1
   70 IV=K
      RETURN
   80 K=3
      COL(15)=2.0
      IF(PT(2)-3.0)170 ,90  ,90
   90 IF(PT(K)-2500.0)110 ,110 ,100
  100 I3=5
      GOTO70
  110 IF(PT(K))150 ,150 ,120
  120 CCOFE = ECO(1)
      IF(PT(K-1)-4.0) 140 ,130 ,130
  130 CCOFE = CCOFE - CTOFE + CTOFEN
  140 IF(PT(K) - CCOFE ) 150 ,150 ,160
  150 M=PT(K-1)
      PNBC(M)=PNBC(M)+1.0
      PT(K)=0.0
      I3=3
      GOTO70
  160 IF(K-3)170 ,170 ,210
  170 COL(15)=3.0
      K=15
      IF(PT(14)-2.0)180,180,90
  180 I3=2
      GOTO70
  190 L=14
      DO200 M=5,7
         PT(M)=PNIDK(L)
         PT(M+12)=PNIDK(L+3)
  200 L=L+1
      PT(11)=PNIDK(12)
      PT(12)=PNIDK(6)
      I=4
      K=39
      COL(15)=5.0
      GO TO 30
  210 I1=3
  220 K=12*I1-33
      IF(I1-4)230 ,240 ,250
  230 I2=-1
      GO TO 280
  240 I2=0
      GO TO 280
  250 IF(I1-5)240 ,270 ,260
  260 I3=4
      GO TO 70
  270 I2=1
  280 IF(PT(K))290 ,300 ,290
  290 CALL PSTOR
  300 I1=I1+1
      GO TO 220
      END
