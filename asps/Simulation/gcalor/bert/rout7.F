* $Id: rout7.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT7
C
#include "geant321/crandm.inc"
C
#include "gcalor/cbert.inc"
      SAVE
      I3=0
      IF(CURR(1)-3.0)20 ,50 ,10
   10 IF(CURR(1)-5.0)60 ,40 ,20
C     PROTON, NEUTRON NOT PERMITTED
   20 I3=-1
   30 RETURN
   40 IT=7
      IFCA=5
C     PI MESON - (5)
      ABSEC=PMAC(MED)
C     PIM +PP ABS.  TYOR=PMAPP(20021)
      GOTO90
   50 IT=10
      IFCA=3
C     TYOR=PPAN(20004)  PIP-NN ABS. ENERGY CORRECTION PIMESON +
      ABSEC=PPAN(MED)
      GOTO100
   60 VALUE1 = RANDC(ISEED)
      IF(VALUE1-PPNNA)70 ,80 ,80
   70 IT=8
      IFCA=4
C     PNANN(20015)=TYOR  PIN-NN ABS  PIMESON 0
      ABSEC=-HVN(MED)
      GOTO100
   80 IT=9
      IFCA=2
C     PNAPP(20011)=TYOR  PIN+PP ABS.  PIMESON 0
      ABSEC=-HVP(MED)
   90 STRKP=-1.0
      E(1)=WKRPN(MED)*RCPMV+PM(1)
      GOTO110
  100 STRKP=-2.0
      E(1)=WKRPN(MED+3)*RCPMV+PM(1)
  110 IF(INC)130,120,130
  120 CALLP1CLC
      GOTO30
  130 CALLP1CLI
      GOTO30
      END
