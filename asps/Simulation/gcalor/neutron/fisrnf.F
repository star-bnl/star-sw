* $Id: fisrnf.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
C*********************************************************************
      FUNCTION FISRNF(A,B)
C*********************************************************************
C Sample secondary fission neutron energy from Watt spectrum
C taken from ORNL/TM-7631
C CZ 3/11/92
      DIMENSION RNDM(3)
C
      CALL GRNDM(RNDM,3)
      Z=SQRT(-ALOG(RNDM(1)))
      S=6.28319*RNDM(2)
      ALOGR3=ALOG(RNDM(3))
      X=SQRT(A*B)/2.
      E1=A*((Z*COS(S)+X)**2-ALOGR3)
C--  E2=A*((Z*SIN(S)+X)**2-ALOGR3)
C distribution of E1 and E2 are identical
      FISRNF = E1
      RETURN
      END
