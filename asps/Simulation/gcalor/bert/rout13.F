* $Id: rout13.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT13
      SAVE
#include "gcalor/cbert.inc"
C
      I3=0
      IF(IV)10 ,50 ,50
   10 IF(XABS)20 ,50 ,20
   20 IF(IFCA-2)30 ,70 ,60
   30 IN=0
   40 I3=1
      GOTO90
   50 CALLSIGNEX
      IF(IFC-12)120,120,130
   60 IF(IFCA-6)70 ,30 ,100
   70 IN=0
   80 I3=-1
   90 RETURN
  100 IF(IFCA-8)30 ,110,110
  110 IN=1
      GOTO40
  120 IN=0
      GOTO90
  130 IF(IFC-18)140,140,150
  140 IN=-1
      GOTO90
  150 IN=1
      GOTO90
      END
