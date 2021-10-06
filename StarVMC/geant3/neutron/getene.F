* $Id: getene.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE GETENE(EN,D1,D2,N)
C sample N times secondary energy distribution and
C store in ENE(*)
#include "geant321/mconst.inc"
#include "geant321/mnutrn.inc"
C
      DIMENSION D1(*),D2(*)
C
      DO 10 I=1,N
         CALL SECEGY(EX,D1,EN,D2)
         ENE(I) = EX
  10  CONTINUE
      RETURN
      END

