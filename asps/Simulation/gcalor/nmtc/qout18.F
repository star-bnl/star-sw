* $Id: qout18.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE QOUT18
      IMPLICIT REAL *8  (A-H,O-Z)
#include "gcalor/cbert3.inc"
#include "gcalor/cisob2.inc"
      SAVE
      GOTO(10  ,20  ,80  ,120 ,80  ,100 ),I3
   10 I=3
      COL(15)=1.0
      K=27
      GOTO30
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
      CALL CALQDK
      IF(K-27)60  ,50  ,60
   50 PT(15)=PT(15)+((PNIDK(12)-PNIDK( 6))/RCPMV)
   60 PT(K)=PT(K)+((PNIDK(13)-DNCMS)/RCPMV)
      I3=1
   70 IV=K
      RETURN
   80 COL(15)=3.0
      K=15
      IF(PT(14)-2.0)90 ,90 ,120
   90 I3=2
      GOTO70
  100 L=14
      DO110 M=5,7
         PT(M)=PNIDK(L)
         PT(M+12)=PNIDK(L+3)
  110 L=L+1
      PT(11)=PNIDK(12)
      PT(12)=PNIDK(6)
      I=4
      K=39
      COL(15)=5.0
      GOTO30
  120 I1=3
  130 K=12*I1-33
  140 IF(I1-4)150 ,160 ,170
  150 I2=-1
      GOTO200
  160 I2=0
      GOTO200
  170 IF(I1-5)160 ,190 ,180
  180 I3=4
      GOTO70
  190 I2=1
  200 IF(PT(K))210 ,220 ,210
  210 CALL CQSTOR
  220 I1=I1+1
      GOTO130
      END
