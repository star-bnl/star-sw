* $Id: rout7a.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT7A
#include "gcalor/cbert.inc"
C
      SAVE
   10 I1=-1
      CALLSPISOM
      GOTO(20 ,30 ,20 ,20 ,30 ),IFCA
   20 VALUE1=SPACE(MED+3)-7.0
      GOTO40
   30 VALUE1=SPACE(MED+9)-7.0
   40 IF(VALUE1)50 ,70 ,70
   50 I3=2
   60 RETURN
   70 IF((VALUE1*2.0*RCPMV)-E(2))10 ,10 ,80
   80 PM(3)=DNCMS
      PM(2)=2.0*DNCMS
      E(2)=PM(2)+E(2)
      VALUE1=EX
      IF(MED-2)90 ,100,110
   90 I3=3
      GOTO60
  100 I3=4
      GOTO60
  110 IF(INC)120,170,120
  120 IF(ISW(1))140,130,140
  130 I3=5
      GOTO60
  140 IF(ISW(2))150,160,150
  150 I3=6
      GOTO60
  160 I3=9
      GOTO60
  170 IF(ISW(1))190,180,190
  180 I3=7
      GOTO60
  190 IF(ISW(2))200,210,200
  200 I3=1
      GOTO60
  210 I3=8
      GOTO60
      END
