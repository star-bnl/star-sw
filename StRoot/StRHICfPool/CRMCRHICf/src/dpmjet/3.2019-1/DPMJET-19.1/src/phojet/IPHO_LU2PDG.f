
      INTEGER FUNCTION IPHO_LU2PDG(Lukf)
C**********************************************************************
C
C    conversion of JETSET KF code to PDG code
C
C**********************************************************************
      IMPLICIT NONE
      INTEGER i , lu2pd , Lukf , NTAB
      SAVE 
      PARAMETER (NTAB=10)
      DIMENSION lu2pd(2,NTAB)
      DATA lu2pd/4232 , 4322 , 4322 , 4232 , 3212 , 3122 , 3122 , 3212 , 
     &     30553 , 20553 , 30443 , 20443 , 20443 , 10443 , 10443 , 0 , 
     &     511 , 0 , 10551 , 551/
C
      DO i = 1 , NTAB
         IF ( lu2pd(1,i).EQ.Lukf ) THEN
            IPHO_LU2PDG = lu2pd(2,i)
            RETURN
         END IF
      END DO
      IPHO_LU2PDG = Lukf
 
      END FUNCTION
