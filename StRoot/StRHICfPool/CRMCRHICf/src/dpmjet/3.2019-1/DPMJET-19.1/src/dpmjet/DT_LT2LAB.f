
      SUBROUTINE DT_LT2LAB
 
C***********************************************************************
C Lorentz-transformation to lab-system. This subroutine scans DTEVT1   *
C for final state particles/fragments defined in nucleon-nucleon-cms   *
C and transforms them to the lab.                                      *
C This version dated 07.01.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i
      DOUBLE PRECISION pe , pz
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
 
      IF ( (NPOint(4).EQ.0) .OR. (NHKk.LT.NPOint(4)) ) RETURN
      DO i = NPOint(4) , NHKk
         IF ( (ABS(ISThkk(i)).EQ.1) .OR. (ISThkk(i).EQ.1000) .OR. 
     &        (ISThkk(i).EQ.1001) ) THEN
            CALL DT_LTNUC(PHKk(3,i),PHKk(4,i),pz,pe,-3)
            PHKk(3,i) = pz
            PHKk(4,i) = pe
         END IF
      END DO
 
      END SUBROUTINE
