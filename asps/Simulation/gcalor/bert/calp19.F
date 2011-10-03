* $Id: calp19.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CALP19
#include "geant321/crandm.inc"
#include "gcalor/cbert.inc"
      SAVE
C
C
      UNIV = RANDC(ISEED)
      PT(2)=1.0
      PT(26)=1.0
      PT(14)=3.0
      PT(16)=POMS
      IF(ISW(12))10,10,140
   10 IF(UNIV-.25)20,20,110
   20 IF(ISW(4))40,30,40
   30 PT(2)=2.0
   40 UNIV = RANDC(ISEED)
      IF(UNIV-6.6666667D-1)50,50,90
   50 PT(14)=4.0
   60 IF(ISW(4))80,70,80
   70 PT(26)=2.0
   80 GO TO 240
   90 PT(16)=PNMS
      IF(ISW(4))70,100,70
  100 PT(14)=5.0
      GOTO80
  110 IF(ISW(4))130,120,130
  120 PT(26)=2.0
      GOTO90
  130 PT(2)=2.0
      PT(16)=PNMS
      GOTO80
  140 IF(UNIV-.5)150,150,190
  150 IF(ISW(4))160,170,160
  160 PT(2)=2.0
  170 UNIV = RANDC(ISEED)
      IF(UNIV-3.3333333D-1)90,90,180
  180 PT(14)=4.0
      GOTO60
  190 IF(ISW(4))210,200,210
  200 PT(2)=2.0
  210 UNIV = RANDC(ISEED)
      IF(UNIV-6.6666667D-1)220,220,230
  220 PT(14)=4.0
      IF(ISW(4))70,80,70
  230 PT(16)=PNMS
      IF(ISW(4))100,70,100
  240 RETURN
      END
