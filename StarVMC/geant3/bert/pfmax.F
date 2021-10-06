* $Id: pfmax.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE PFMAX
#include "gcalor/cbert.inc"
      REAL * 8 WK
      SAVE
C
      I=I2
      IF(CURR(1)-2.0)20,10,10
   10 I=I+3
   20 WK=WKRPN(I)
      IF(WK-160.0)30,30,50
   30 FMAX(1)=0.176D-24
      FMAX(2)=0.52D-24
      FMAX(3)=0.0
      FMAX(4)=0.0
   40 RETURN
   50 CALL CBOVER(WK,DNCMS,UNIV)
      IF(WK-400.0)60,110,110
   60 IF(WK-300.0)70,100,100
   70 IF(WK-200.0)80,90,90
   80 FMAX(3)=1.95D-27*UNIV
C     3 AND 4 AT 465 MEV
      FMAX(4)=1.7D-27*UNIV
      FMAX(1)=0.103D-24
      FMAX(2)=0.313D-24
      GOTO40
   90 FMAX(1)=0.09D-24
C     1 AND 2 AT 35 MEV
      FMAX(2)=0.26D-24
      FMAX(3)=11.4D-27*UNIV
C     3 AND 4 AT 630 MEV
      FMAX(4)=11.2D-27*UNIV
      GOTO40
  100 FMAX(1)=28.0D-27*UNIV
C     1 AND 2 AT 100 MEV
      FMAX(2)=0.073D-24
      FMAX(3)=20.0D-27*UNIV
C     3 AND 4 AT 780 MEV
      FMAX(4)=14.0D-27*UNIV
      GOTO40
  110 FMAX(1)=27.2D-27*UNIV
C     1 AND 2 AT 155 MEV
      FMAX(2)=48.0D-27*UNIV
      FMAX(3)=22.6D-27*UNIV
C     3 AND 4 AT 1020 MEV
      FMAX(4)=14.0D-27*UNIV
      GOTO40
C     FMAX(1)=(P-P)S---(2)=(P-N)S---(3)=(P-P)S.P.---(4)=(P-N)S.P.
      END
