
      SUBROUTINE DT_DFERMI(Gpart)
 
C***********************************************************************
C Find largest of three random numbers.                                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , g , Gpart
      INTEGER i
      SAVE 
 
      DIMENSION g(3)
 
      DO i = 1 , 3
         g(i) = DT_RNDM(Gpart)
      END DO
      IF ( g(3).LT.g(2) ) THEN
         IF ( g(2).LT.g(1) ) THEN
            Gpart = g(1)
         ELSE
            Gpart = g(2)
         END IF
      ELSE IF ( g(3).LT.g(1) ) THEN
         Gpart = g(1)
      ELSE
         Gpart = g(3)
      END IF
 
      END SUBROUTINE
