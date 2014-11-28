* $Id: rout19.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT19
      SAVE
C
#include "gcalor/cbert.inc"
C
      PT(3)=PT(3)+((PT(11)-PT(12))/RCPMV)
C     COLLISION ALLOWED
      K=3
   10 IF(PT(K)-2500.0)30  ,30  ,20
   20 I3=1
      GO TO 90
   30 IF(PT(K))70  ,70  ,40
   40 CCOFE = ECO(1)
      IF(PT(K-1)-4.0) 60  ,50  ,50
   50 CCOFE = CCOFE - CTOFE + CTOFEN
   60 IF(PT(K) - CCOFE) 70  ,70  ,170
   70 PT(K)=0.0
      IF(PT(K-1)-3.0)80 ,110 ,100
   80 I3=-1
   90 RETURN
  100 IF(PT(K-1)-5.0)110 ,110 ,80
  110 M=PT(K-1)
      PNBC(M)=PNBC(M)+1.0
      GOTO140
  120 I2=2
  130 I1=(K/12)+3
      CALLPSTOR
  140 IF(K-15)150 ,160 ,190
  150 K=15
      IF(PT(15))160 ,160 ,120
  160 K=27
      PT(27)=PT(27)+((PNIDK(12)-PT(K+1))/RCPMV)
      GOTO10
  170 IF(K-15)120 ,180 ,180
  180 I2=0
      GOTO130
  190 IF(K-27)80 ,200 ,210
  200 IF(PT(39))210 ,210 ,220
  210 I3=0
      GOTO90
  220 I2=1
      K=39
      GOTO130
      END
