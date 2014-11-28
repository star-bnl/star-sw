* $Id: calgeo.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CALGEO
#include "gcalor/cbert.inc"
      REAL * 8 T1,T2,T3,T4,T5,T6,TEMP,TEMPO
      SAVE
C
      I1=0
      T1=OUT(2)*OUT(2)
C     R1SQ
      T2=OUT(3)*OUT(3)
C     (R1+1)SQ
      T3=OUT(4)*OUT(4)
C     (R1+2)SQ
      T4=2.0*T3
C     2(R1+2)SQ
   10 T5=XI(1)*XI(1)+XI(2)*XI(2)+XI(3)*XI(3)
C     T5=R SQ
      GO TO(20 ,70 ,130,190),MED
   20 T6=T5-T1
      IF(T6)240,240,30
C     MED=1
   30 TEMP=T1
   40 IF((T6/TEMP)-5.0D-6)50 ,50 ,230
   50 DO60 I=1,3
         XI(I)=XI(I)*9.99995D-1
   60 CURR(I+3)=XI(I)
      GOTO10
   70 T6=T5-T1
C     MED=2
      IF(T6)80 ,80 ,120
   80 TEMP=T1
   90 IF(5.0D-6+(T6/TEMP))230,100,100
  100 DO110 I=1,3
         XI(I)=XI(I)*10.00005D-1
  110 CURR(I+3)=XI(I)
      GOTO10
  120 T6=T5-T2
      TEMP=T2
      IF(T6)240,240,40
  130 T6=T5-T2
C     MED=3
      IF(T6)140,140,150
  140 TEMP=T2
      GOTO90
  150 T6=T5-T3
      IF(T6)240,240,160
  160 TEMP=T3
C****DUMMY  IF ST. FOLLOWS TO KEEP ST. 175
      IF (TEMP.NE.T3) GO TO 170
      GOTO40
  170 IF(XI(2))230,180,230
C     MED=4
  180 IF(CURR(5))230,190,230
  190 T6=T5-T3
      IF(T6)200,200,210
  200 TEMP=T3
      GOTO90
  210 T6=T5-T4
      IF(T6)240,240,220
  220 TEMP=T4
      GOTO40
  230 I1=-1
      GOTO290
  240 T4=XI(1)*DCOS(1)+XI(2)*DCOS(2)+XI(3)*DCOS(3)
C     T4=-B=-RCOS(THETA)=SUM OF XI(I)*DCOS(I)
      T6=T4*T4
C     T5=R SQ.
      T6=T5-T6
C     T6=R SQ.-B SQ.
      IF(T3-T6)230,250 ,250
  250 T3=DSQRT(T3-T6)
C     T3=A3=SQ.ROOT OF B SQ.-R SQ.+RADIUS3 SQ. SIMILAR
C     FOR T2=A2 ANDT1=A1
      TEMP=T2-T6
      T2=DSQRT(DABS(TEMP))
      TEMPO=T1-T6
      T1=DSQRT(DABS(TEMPO))
      DO260 I=1,6
  260 D(I)=0.0
      GOTO(270,300,360,420),MED
  270 IF(TEMP)230,280,280
  280 D(4)=T1-T4
C     B+A1
      D(5)=T2-T1
C     A2-A1
      D(6)=T3-T2
C     A3-A2
  290 RETURN
  300 IF(TEMP)230,310,310
  310 D(6)=T3-T2
  320 IF(T4)340,330,330
  330 D(3)=T2-T4
C     B+A2
      GOTO290
  340 IF(TEMPO)330,350,350
  350 D(3)=-(T4+T1)
C     B-A1
      D(4)=T1+T1
C     2A1
      D(5)=T2-T1
C     A2-A1
      GOTO290
  360 IF(T4)380,370,370
  370 D(2)=T3-T4
C     B+A3
      GOTO290
  380 IF(TEMP)370,390,390
  390 D(2)=-(T4+T2)
C     B-A2
      D(6)=T3-T2
C     A3-A2
      IF(TEMPO)400,410,410
  400 D(3)=T2+T2
C     2A2
      GOTO290
  410 D(3)=T2-T1
C     A2-A1
      D(5)=D(3)
      D(4)=T1+T1
C     2A1
      GOTO290
  420 D(1)=-(T4+T3)
  430 IF(TEMP)440,450,450
  440 D(2)=T3+T3
      GOTO290
  450 D(2)=T3-T2
      D(6)=D(2)
C     B-A3, A3-A2,REGION 4
      IF(TEMPO)470,460,460
  460 D(3)=T2-T1
C     A2-A1
      D(5)=D(3)
      D(4)=T1+T1
      GOTO290
  470 D(3)=T2+T2
C     2A2
      GOTO290
      END
