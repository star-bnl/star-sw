
      INTEGER FUNCTION IPHO_PDG2LU(Ipdg)
C**********************************************************************
C
C    conversion of PDG code to JETSET code
C
C**********************************************************************
      IMPLICIT NONE
      INTEGER i , Ipdg , lu2pd , NTAB
      SAVE 
      PARAMETER (NTAB=8)
      DIMENSION lu2pd(2,NTAB)
      DATA lu2pd/4232 , 4322 , 4322 , 4232 , 3212 , 3122 , 3122 , 3212 , 
     &     30553 , 20553 , 30443 , 20443 , 20443 , 10443 , 10551 , 551/
C
      DO i = 1 , NTAB
         IF ( lu2pd(2,i).EQ.Ipdg ) THEN
            IPHO_PDG2LU = lu2pd(1,i)
            RETURN
         END IF
      END DO
      IPHO_PDG2LU = Ipdg
 
      END FUNCTION
