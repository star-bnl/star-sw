* $Id: cstore.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CSTORE
#include "gcalor/cbert.inc"
      REAL * 8 SUB1,SUB2
      SAVE
C
      CALL CZERO
      N=I1
      IF(I3)200,10,200
   10 K=I2+6
      L=I2
      GOTO(50,50,50,20,50),I5
   20 IF(I1-5)30,230,30
   30 MM=-6
   40 L=K
      N=N-1
   50 DO110 I=1,N,2
         ID=I
         CE(I+1)=FMAX(I)*1.0D30*SPACE(K)
         CE(I+2)=FMAX(I+1)*1.0D30*SPACE(L)
         GOTO(110,110,60,60,60),I5
   60    M=K
         K=L
         GOTO(110,110,100,80,70),I5
   70    IF(ID-2)110,110,90
   80    K=M+MM
         MM=-MM
         L=K
         GOTO110
   90    K=I2
  100    L=M
  110 CONTINUE
      GOTO(120,120,120,220,120),I5
  120 SIGN=0.0
      DO130 I=2,8
  130 SIGN=SIGN+CE(I)
      GOTO(140,140,150,250,150),I5
  140 SIGN=SIGN*9.99999D-1
      RETURN
  150 IF(I1-4)160,160,140
  160 IF(I3)170,170,180
  170 SPACE(I2+87)=SIGN
      GOTO190
  180 SPACE(I2+106)=SIGN
  190 FMAX(5)=0.0
      FMAX(6)=0.0
      GOTO140
  200 K=I2
      L=I2+6
      GOTO(50,50,50,210,50),I5
  210 MM=6
      GOTO40
  220 CE(8)=FMAX(7)*1.0D30*SPACE(L)
      GOTO120
  230 SUB1=FMAX(1)*1.0D30
      SUB2=FMAX(2)*1.0D30
      DO240 I=2,N,2
         CE(I)=SUB1*SPACE(K)
         CE(I+1)=SUB2*SPACE(K)
         K=L
  240 L=K+6
      CE(N+1)=SPACE(K)*1.0D30*FMAX(N)
      GOTO120
  250 IF(I1-5)140,260,140
  260 SPACE(I2+68)=SIGN
      FMAX(6)=0.0
      FMAX(7)=0.0
      GOTO140
      END
