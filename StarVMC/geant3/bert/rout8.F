* $Id: rout8.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT8
C
#include "gcalor/cbert.inc"
C
      SAVE
      I3=1
      IF(IV) 20 ,10 ,10
   10 IF(VALUE1-VALUE2) 20 ,20 ,110
   20 IF(ISW(3)) 80 ,30 ,80
   30 IFC=7+IFCC
C     7=BG6E(2461)  8=BG6IA(4026)  NTNT(21626)  BG48X(12762)=19
      IF(IN) 40  ,60 ,40
   40 I3=2
   50 RETURN
   60 C(3)=D(2)
      GO TO 70
   70 I3=3
      GO TO 50
   80 IFC=8+IFCC
      IF(IN)90 ,100,90
   90 I3=4
      GO TO 50
  100 C(3)=D(2)+D(3)+D(4)
      GO TO 70
  110 CALL SIGNEX
      GOTO 50
      END
