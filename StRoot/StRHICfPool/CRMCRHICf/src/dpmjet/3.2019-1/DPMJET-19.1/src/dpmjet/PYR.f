#ifndef FOR_CORSIKA

      DOUBLE PRECISION FUNCTION PYR(Idummy)
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , dummy
      INTEGER Idummy
      SAVE 
 
      dummy = DBLE(Idummy)
      PYR = DT_RNDM(dummy)
      END FUNCTION
#endif
