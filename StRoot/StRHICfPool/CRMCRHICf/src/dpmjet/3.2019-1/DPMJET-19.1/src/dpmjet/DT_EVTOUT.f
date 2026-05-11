
      SUBROUTINE DT_EVTOUT(Mode)
 
C***********************************************************************
C            MODE  = 1  plot content of complete DTEVT1 to out. unit   *
C                    3  plot entries of extended DTEVT1 (DTEVT2)       *
C                    4  plot entries of DTEVT1 and DTEVT2              *
C This version dated 11.12.94 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER irange , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
 
      DIMENSION irange(NMXHKK)
 
 
      IF ( Mode.EQ.2 ) RETURN
      CALL DT_EVTPLO(irange,Mode)
 
      END SUBROUTINE
