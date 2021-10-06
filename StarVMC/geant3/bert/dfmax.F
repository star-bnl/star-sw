* $Id: dfmax.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE DFMAX
      SAVE
#include "gcalor/cbert.inc"
      REAL*8 WK
C
      I=I2
      IF(CURR(1)-2.0)20,10,10
   10 I=I+3
   20 WK=WKRPN(I)
   30 CALL CBOVER(WK,DNCMS,UNIV)
      IF(WK-560.0)40,40,60
   40 FMAX(1)=27.2D-27*UNIV
C     820 MEV
C     (P-P)S
      FMAX(2)=38.0D-27*UNIV
C     230 MEV
C     (P-N)S
      FMAX(3)=22.6D-27*UNIV
C     1020 MEV
C     (P-P)S.P.
      FMAX(4)=14.0D-27*UNIV
C     750 MEV
C     (P-N)S.P.
      FMAX(5)=0.0
C     (P-P)D.P.
      FMAX(6)=0.0
C     (P-N)D.P.)
   50 RETURN
   60 IF(WK-800.0)70,90,90
   70 FMAX(2)=37.0D-27*UNIV
C     250 MEV
      FMAX(5)=1.9D-27*UNIV
C     5 AN6 AT 1380 MEV
      FMAX(6)=9.0D-27*UNIV
   80 FMAX(1)=27.2D-27*UNIV
C     820 MEV
      FMAX(3)=22.6D-27*UNIV
C     1020
      FMAX(4)=14.0D-27*UNIV
C     750
      GO TO 50
   90 IF(WK-1680.0)100,110,110
  100 FMAX(2)=33.0D-27*UNIV
C     400
      FMAX(5)=10.8D-27*UNIV
C     5 AND 6 AT 2600
      FMAX(6)=17.4D-27*UNIV
      GO TO 80
  110 FMAX(1)=26.3D-27*UNIV
C     1000
      FMAX(2)=24.7D-27*UNIV
C     1000
      FMAX(3)=22.6D-27*UNIV
C     1020
      FMAX(4)=13.5D-27*UNIV
C     1000
      FMAX(5)=18.0D-27*UNIV
      FMAX(6)=23.6D-27*UNIV
C     3500
      GO TO 50
      END
