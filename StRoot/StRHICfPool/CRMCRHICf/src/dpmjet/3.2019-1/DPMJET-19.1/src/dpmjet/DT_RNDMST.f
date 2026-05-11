
      SUBROUTINE DT_RNDMST(Na1,Na2,Na3,Nb1)
 
      IMPLICIT NONE
      INTEGER ii1 , ii2 , ma1 , ma2 , ma3 , mat , mb1 , Na1 , Na2 , 
     &        Na3 , Nb1
      DOUBLE PRECISION s , t
      SAVE 
 
C random number generator
      INCLUDE 'inc/dtrand'
 
      ma1 = Na1
      ma2 = Na2
      ma3 = Na3
      mb1 = Nb1
      I = 97
      J = 33
      DO ii2 = 1 , 97
         s = 0
         t = 0.5D0
         DO ii1 = 1 , 24
            mat = MOD(MOD(ma1*ma2,179)*ma3,179)
            ma1 = ma2
            ma2 = ma3
            ma3 = mat
            mb1 = MOD(53*mb1+1,169)
            IF ( MOD(mb1*mat,64).GE.32 ) s = s + t
            t = 0.5D0*t
         END DO
         U(ii2) = s
      END DO
      C = 362436.0D0/16777216.0D0
      CD = 7654321.0D0/16777216.0D0
      CM = 16777213.0D0/16777216.0D0
      END SUBROUTINE
