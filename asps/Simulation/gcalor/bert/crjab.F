* $Id: crjab.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CRJAB(K1,PP)
#include "gcalor/cbert.inc"
#include "geant321/crandm.inc"
      REAL*8 PP(380)
C
      CALL CRDET(K1,PP(1),VALUE1)
      VALUE1=(PXYZ(1)*PXYZ(2)+PXYZ(5)*PXYZ(6)+PXYZ(9)*PXYZ(10))
     1/E(1)
C     P1.P2/E(1)
      VALUE2=(VALUE1/(P2*P2))*((E(2)/DNCMS)-1.0)-(1.0/DNCMS)
C     S=((P1.P2)/(E1*P2*P2))*((E2/M)-1.0)-1.0/M
      VALUE2=DNCMS*CRDT(1)*DSQRT(P1OE1*P1OE1+2.0*VALUE1
     1*VALUE2+P2*P2*VALUE2*VALUE2)/(E(2)*P1OE1*ANY)
C     (M)(C.S)(J**2+2S(P1.P2)/E1+(P2)(P2)(S)(S)
CZ changed 20.june 92 CZ
      IF(VALUE2.GT.1.0) VALUE2 = 1.0
C     THIS TESTS SAMPLING TECH.TO ENSURE FMAXS WERE SELECTED SO THAT
C     VALUE2 LTE ONE.
      VALUE1 = RANDC(ISEED)
      RETURN
      END
