* $Id: calxyi.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CALXYI(II,JJ,KK)
C
#include "gcalor/cbert.inc"
C
      REAL * 8 W1,W2,W3,W4,W5,W6
      SAVE
C
      W1=S(II)*1.0D30
      W2=S(II+1)*1.0D30
      IF(IABS(IV)-1)10,20,10
   10 W3=SPACE(JJ)*1.0D30
      W4=SPACE(JJ+1)*1.0D30
      W5=SPACE(JJ+2)*1.0D30
      W6=SPACE(JJ+3)*1.0D30
      LL=II+2
      IF(IV)40,60,40
   20 W3=S(II+2)*1.0D30
      W4=S(II+3)*1.0D30
      W5=SPACE(JJ)*1.0D30
      W6=SPACE(JJ+1)*1.0D30
      LL=II+4
      IF(IV)50,30,30
   30 NM=7
      I7=1
   40 MM=7
      NN=1
      GO TO 70
   50 NM=1
      I7=7
   60 MM=1
      NN=7
   70 DO100 I=1,3
         S(LL)=W1*SPACE(MM)
         S(LL+1)=W2*SPACE(NN)
         IF(IABS(IV)-1)90,80,90
   80    S(LL+2)=W3*SPACE(NM)
         S(LL+3)=W4*SPACE(I7)
         LL=LL+2
         NM=NM+1
         I7=I7+1
   90    MM=MM+1
         NN=NN+1
  100 LL=LL+2
      IF(IV)110,120,130
  110 NM=7
      I7=1
      GOTO170
  120 MM=1
      NN=7
      NM=7
      I7=7
      GOTO 150
  130 IF(IV-1)160,160,140
  140 MM=7
      NN=1
      NM=1
      I7=7
  150 LL=JJ+4
      GOTO180
  160 NM=1
      I7=7
  170 LL=JJ
  180 DO210 I=1,3
         SPACE(LL+2)=W5*SPACE(NM)
         SPACE(LL+3)=W6*SPACE(I7)
         IF(IABS(IV)-1)190,200,190
  190    SPACE(LL)=W3*SPACE(MM)
         SPACE(LL+1)=W4*SPACE(NN)
         LL=LL+2
         MM=MM+1
         NN=NN+1
  200    NM=NM+1
         I7=I7+1
  210 LL=LL+2
      LL=KK+2
      IF(IABS(IV)-1)220,250,220
  220 MM=JJ+4
      NN=II+2
      DO230 I=KK,LL
         SPACE(I)=SPACE(MM)+SPACE(MM+1)+SPACE(MM+2)+SPACE(MM+3) +S(NN)+
     +   S(NN+1)
         MM=MM+4
  230 NN=NN+2
  240 RETURN
  250 MM=II+4
      NN=JJ+2
      DO260 I=KK,LL
         SPACE(I)=SPACE(NN)+SPACE(NN+1)+S(MM)+S(MM+1)+S(MM+2)+S(MM+3)
         MM=MM+4
  260 NN=NN+2
      GOTO240
      END
