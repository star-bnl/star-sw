* $Id: cadcpr.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CADCPR(W,LK)
#include "gcalor/cbert.inc"
C
#include "geant321/crandm.inc"
      REAL*8 W(60),Z(24)
      REAL*8 FRACT,R,SUM,FLTI
      SAVE
C
C PDCI HAS KK=12, PDCH HAS KK=11
      I2=0
      KK=LK
      K=1
      DO10 I=1,5
         IF(W(K).GE.RLKE)GOTO40
   10 K=K+KK
   20 I2=1
C ERROR RETURN
   30 RETURN
   40 DO 50 L = 1,KK
         Z(L+KK)=W(K)
         Z(L)=W(K-KK)
   50 K=K+1
      SUM=0.0D0
      R = RANDC(ISEED)
      FRACT=(RLKE-Z(1))/(Z(KK+1)-Z(1))
      DO 60 I = 2,KK
         SUM=SUM+Z(I)+((Z(I+KK)-Z(I))*FRACT)
         IF(R.LT.SUM) GO TO 70
   60 CONTINUE
      GO TO 20
C ERROR
   70 R = RANDC(ISEED)
      I1=I
      IF(KK.GT.11) GO TO 90
      IF(I1.GT.2) GO TO 80
      CST=4.0D-1*R
      GO TO 150
   80 I1=I1+1
   90 FLTI=DFLOAT(I1-2)+R
  100 GO TO (20,110,110,110,120,120,130,130,130,130,140,140),I1
  110 CST=2.0D-1*(FLTI)
      GO TO 150
  120 CST=3.0D-1+1.0D-1*(FLTI)
      GO TO 150
  130 CST=6.0D-1+4.0D-2*(FLTI)
      GO TO 150
  140 CST=7.8D-1+2.0D-2*(FLTI)
  150 R = RANDC(ISEED)
      IF(R.GT.5.0D-1) GO TO 30
      CST=-(CST)
      GO TO 30
      END
