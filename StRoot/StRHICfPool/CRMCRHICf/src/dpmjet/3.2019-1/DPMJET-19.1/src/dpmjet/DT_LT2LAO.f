
      SUBROUTINE DT_LT2LAO
 
C***********************************************************************
C Lorentz-transformation to lab-system. This subroutine scans DTEVT1   *
C for final state particles/fragments defined in nucleon-nucleon-cms   *
C and transforms them back to the lab.                                 *
C This version dated 16.11.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , nend , nob
      DOUBLE PRECISION pe , pz
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
 
      nend = NHKk
      NPOint(5) = NHKk + 1
      IF ( (NPOint(4).EQ.0) .OR. (nend.LT.NPOint(4)) ) RETURN
      DO i = NPOint(4) , nend
C     DO 1 I=1,NEND
         IF ( (ABS(ISThkk(i)).EQ.1) .OR. (ISThkk(i).EQ.1000) .OR. 
     &        (ISThkk(i).EQ.1001) ) THEN
            CALL DT_LTNUC(PHKk(3,i),PHKk(4,i),pz,pe,-3)
            nob = NOBam(i)
            CALL DT_EVTPUT(ISThkk(i),IDHkk(i),i,0,PHKk(1,i),PHKk(2,i),
     &                     pz,pe,IDRes(i),IDXres(i),IDCh(i))
            IF ( (ISThkk(i).EQ.1000) .OR. (ISThkk(i).EQ.1001) ) THEN
               ISThkk(i) = 3*ISThkk(i)
               NOBam(NHKk) = nob
            ELSE
               IF ( ISThkk(i).EQ.-1 ) NOBam(NHKk) = nob
               ISThkk(i) = SIGN(3,ISThkk(i))
            END IF
            JDAhkk(1,i) = NHKk
         END IF
      END DO
 
      END SUBROUTINE
