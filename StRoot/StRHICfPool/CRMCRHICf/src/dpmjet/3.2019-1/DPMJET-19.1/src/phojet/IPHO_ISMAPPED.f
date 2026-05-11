
      INTEGER FUNCTION IPHO_ISMAPPED(Ipdg)
C*********************************************************************
 
C     Returns PDG ID of particle to which ipdg is mapped. If it is not
C     mapped to anything the original ipdg is returned
 
C     input:   PDG ID of incoming particle
 
C     output:  PDG ID under which the particle is tracked in
C              initialization
 
C*********************************************************************
      IMPLICIT NONE
 
      INTEGER Ipdg , i
 
      INCLUDE 'inc/pobeam'
 
      IPHO_ISMAPPED = Ipdg
      IF ( NMApp.LT.1 ) RETURN
      DO i = 1 , NMApp
         IF ( Ipdg.EQ.MPMapp(1,i) ) THEN
            IPHO_ISMAPPED = MPMapp(2,i)
            GOTO 99999
         END IF
        ! The array should not contain any zeros due in between values
         IF ( MPMapp(1,i).EQ.0 ) GOTO 99999
      END DO
 
99999 END FUNCTION
