
      SUBROUTINE DT_DPOLI(Cs,Si)
 
      IMPLICIT NONE
      DOUBLE PRECISION Cs , DT_RNDM , Si , u
      SAVE 
 
      u = DT_RNDM(Cs)
      Cs = DT_RNDM(u)
      IF ( u.LT.0.5D0 ) Cs = -Cs
      Si = SQRT(1.0D0-Cs*Cs+1.0D-10)
 
      END SUBROUTINE
