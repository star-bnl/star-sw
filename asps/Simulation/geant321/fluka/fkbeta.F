#include "geant321/pilot.h"
*
*=== beta renamed
*
      FUNCTION FKBETA(X1,X2,BET)
#include "geant321/dblprc.inc"
#include "geant321/dimpar.inc"
#include "geant321/iounit.inc"
      AX=0.D0
      BETX1=BET*X1
      IF(BETX1.LT.70.D0) AX=-1.D0/BET**2*(BETX1+1.D0)*EXP(-BETX1)
      AY=1.D0/BET**2*(BET*X2+1.D0)*EXP(-BET*X2)
      FKBETA=AX+AY
      END
