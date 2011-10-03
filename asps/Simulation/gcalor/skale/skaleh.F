* $Id: skaleh.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE SKALEH(IBERT,ITINC,HSIG,EINC,NOFAS,ITFAS,EFAS,ALPHAS,
     +  BETFAS,GAMFAS,EHICUT)
C
#include "geant321/cmass.inc"
      DIMENSION ITFAS(*), EFAS(*) , ALPHAS(*) , BETFAS(*) , GAMFAS(*)
      DIMENSION R(60)
      SAVE
C
      ITYPE = ITINC + 1
      CALL CPCOL(IBERT,ITYPE,HSIG,EHICUT,NOFAS,ITFAS,EFAS,ALPFAS,BETFAS,
     +  GAMFAS)
      IF(NOFAS.LE.0) RETURN
CZ simple scaling for H-collision, conserve energy  19 Nov. 1992
      EI=EINC+PMASS(ITINC+1)
      ESUMF = 0.0
      DO 10 I=1,NOFAS
         ESUMF=ESUMF+PMASS(ITFAS(I)+1)+EFAS(I)
   10 CONTINUE
      EDIV = EI-ESUMF
      IF(EDIV.LE.0.0) RETURN
      CALL GRNDM(R,NOFAS)
      RS=0.0
      DO 20 I=1,NOFAS
         RS=RS+R(I)
   20 CONTINUE
      DO 30 I=1,NOFAS
         EFAS(I) = EFAS(I)+R(I)/RS*EDIV
   30 CONTINUE
      RETURN
      END
