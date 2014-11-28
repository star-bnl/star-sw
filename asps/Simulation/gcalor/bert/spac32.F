* $Id: spac32.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE SPAC32(I)
#include "gcalor/cbert.inc"
      SAVE
C
      EX=0.0
      I4=5
      SIGN=9.99999D-1*SPACE(I)
      IF(I-31)10,20,30
   10 I2=18
      I3=3
      GOTO90
   20 I2=22
      I3=5
      GOTO90
   30 IF(I-41)40,50,50
   40 I2=26
      I3=7
      GOTO90
   50 I4=3
      IF(I-42)60,70,80
   60 I2=35
      I3=13
      GOTO90
   70 I2=37
      I3=17
      GOTO90
   80 I2=39
      I3=21
   90 CALLCABG6B
      CALLSIGNEX
      RETURN
      END
