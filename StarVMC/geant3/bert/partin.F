* $Id: partin.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE PARTIN
#include "gcalor/cbert.inc"
C
      SAVE
      IF(D(4))10 ,20 ,10
   10 IPEC(10)=IPEC(10)+1
C     NO. OF INC. PARTICLES ON REG.1 ONLY
      GOTO50
   20 IF(D(3))30 ,40 ,30
   30 IPEC(6)=IPEC(6)+1
C     NO. OF INC. PARTICLES ON REG.2 ONLY
      GOTO50
   40 IPEC(1)=IPEC(1)+1
C     NO. OF INC. PARTICLES ON REG.3 ONLY
   50 DO60 I=1,3
   60 ISW(I)=0
C     1=0 WHEN START IN RG.3 OR RG.4
C     2=0 WHEN IN RG.3 NOT PASSING THROUGH RG.1
C     3=0 WHEN IN RG.2 NOT PASSING THROUGH RG.1
      RETURN
      END
