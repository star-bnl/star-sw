
      SUBROUTINE DT_EVTINI
 
C***********************************************************************
C Initialization of DTEVT1.                                            *
C This version dated 15.01.94 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , j , nend
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C event flag
      INCLUDE 'inc/dtevno'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
 
C initialization of DTEVT1/DTEVT2
      nend = NHKk
      IF ( NEVent.EQ.1 ) nend = NMXHKK
      NHKk = 0
      NEVhkk = NEVent
      DO i = 1 , nend
         ISThkk(i) = 0
         IDHkk(i) = 0
         JMOhkk(1,i) = 0
         JMOhkk(2,i) = 0
         JDAhkk(1,i) = 0
         JDAhkk(2,i) = 0
         IDRes(i) = 0
         IDXres(i) = 0
         NOBam(i) = 0
         IDCh(i) = 0
         IHIst(1,i) = 0
         IHIst(2,i) = 0
         DO j = 1 , 4
            PHKk(j,i) = 0.0D0
            VHKk(j,i) = 0.0D0
            WHKk(j,i) = 0.0D0
         END DO
         PHKk(5,i) = 0.0D0
      END DO
      DO i = 1 , 10
         NPOint(i) = 0
      END DO
      CALL DT_CHASTA(-1)
 
C* initialization of DTLTRA
C      IF (NCOMPO.GT.0) CALL DT_LTINI(ID,EPN,PPN,ECM)
 
      END SUBROUTINE
