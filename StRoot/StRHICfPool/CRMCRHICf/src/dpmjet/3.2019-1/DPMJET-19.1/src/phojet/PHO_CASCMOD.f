
      SUBROUTINE PHO_CASCMOD
C*********************************************************************
 
C     Initializes default set of particles for cascade calcualtions
 
C*********************************************************************
      IMPLICIT NONE
      INTEGER i
 
      INCLUDE 'inc/pobeam'
      INCLUDE 'inc/poinou'
      INCLUDE 'inc/podebg'
 
      INTEGER projids(4)
 
      DATA projids/2212 , 2112 , 211 , 321/
C      3122,3112,130,111 /
 
 
      DO i = 1 , 4
         CALL PHO_SETPAR(1,projids(i),0,0.D0)
         CALL PHO_SETPAR(2,2212,0,0.D0)
         CALL PHO_SETPCOMB
         CALL PHO_SETPAR(2,2112,0,0.D0)
         CALL PHO_SETPCOMB
      END DO
 
 
      END SUBROUTINE
