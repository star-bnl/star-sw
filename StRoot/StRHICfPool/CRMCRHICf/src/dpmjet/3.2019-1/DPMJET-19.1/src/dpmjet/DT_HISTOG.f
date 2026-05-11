
      SUBROUTINE DT_HISTOG(Mode)
 
C***********************************************************************
C This version dated 25.03.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , idum , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      LOGICAL lfsp , lrnl
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C event flag used for histograms
      INCLUDE 'inc/dtnorm'
C flags for activated histograms
      INCLUDE 'inc/dthis3'
 
      IEVhkk = NEVhkk
      IF ( Mode.EQ.2 ) THEN
C------------------------------------------------------------------
C filling of histogram with event-record
         ICEvt = ICEvt + 1
 
         DO i = 1 , NHKk
            CALL DT_SWPFSP(i,lfsp,lrnl)
            IF ( lfsp ) THEN
               IF ( IHIspp(1).EQ.1 ) CALL DT_HISTAT(i,2)
               IF ( IHIspp(2).EQ.1 ) CALL DT_HIMULT(2)
            END IF
            IF ( IHIspp(1).EQ.1 ) CALL DT_HISTAT(i,5)
         END DO
 
         IF ( IHIspp(1).EQ.1 ) CALL DT_HISTAT(idum,4)
         RETURN
      ELSE IF ( Mode.EQ.3 ) THEN
C------------------------------------------------------------------
C output
         IF ( IHIspp(1).EQ.1 ) CALL DT_HISTAT(idum,3)
 
         IF ( IHIspp(2).EQ.1 ) CALL DT_HIMULT(3)
         GOTO 99999
      END IF
 
C------------------------------------------------------------------
C initialization
      ICEvt = 0
      IF ( IHIspp(1).EQ.1 ) CALL DT_HISTAT(idum,1)
 
      IF ( IHIspp(2).EQ.1 ) CALL DT_HIMULT(1)
99999 END SUBROUTINE
