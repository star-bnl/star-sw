
      SUBROUTINE DT_EMC2(Ip1,Ip2,Ip3,Ip4,Ip5,Mp,In1,In2,In3,In4,In5,Mn,
     &                   Mode,Ipos,Irej)
 
C***********************************************************************
C             MODE = 1   energy-momentum cons. check                   *
C                  = 2   flavor-cons. check                            *
C                  = 3   energy-momentum & flavor cons. check          *
C                  = 4   energy-momentum & charge cons. check          *
C                  = 5   energy-momentum & flavor & charge cons. check *
C This version dated 16.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION dum , TINY10 , ZERO
      INTEGER i , idum , In1 , In2 , In3 , In4 , In5 , Ip1 , Ip2 , Ip3 , 
     &        Ip4 , Ip5 , Ipos , Irej , irej1 , irej2 , irej3 , Mn , 
     &        Mode , Mp
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ZERO=0.0D0)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
 
      Irej = 0
      irej1 = 0
      irej2 = 0
      irej3 = 0
 
      IF ( (Mode.EQ.1) .OR. (Mode.EQ.3) .OR. (Mode.EQ.4) .OR. 
     &     (Mode.EQ.5) ) CALL DT_EVTEMC(ZERO,ZERO,ZERO,ZERO,1,idum,idum)
      IF ( (Mode.EQ.2) .OR. (Mode.EQ.3) .OR. (Mode.EQ.5) )
     &     CALL DT_EVTFLC(0,idum,1,idum,idum)
      IF ( (Mode.EQ.4) .OR. (Mode.EQ.5) )
     &     CALL DT_EVTCHG(idum,1,idum,idum)
      DO i = 1 , NHKk
         IF ( (ISThkk(i).EQ.Ip1) .OR. (ISThkk(i).EQ.Ip2) .OR. 
     &        (ISThkk(i).EQ.Ip3) .OR. (ISThkk(i).EQ.Ip4) .OR. 
     &        (ISThkk(i).EQ.Ip5) ) THEN
            IF ( (Mode.EQ.1) .OR. (Mode.EQ.3) .OR. (Mode.EQ.4) .OR. 
     &           (Mode.EQ.5) ) CALL DT_EVTEMC(PHKk(1,i),PHKk(2,i),
     &           PHKk(3,i),PHKk(4,i),2,idum,idum)
            IF ( (Mode.EQ.2) .OR. (Mode.EQ.3) .OR. (Mode.EQ.5) )
     &           CALL DT_EVTFLC(IDHkk(i),Mp,2,idum,idum)
            IF ( (Mode.EQ.4) .OR. (Mode.EQ.5) )
     &           CALL DT_EVTCHG(IDHkk(i),2,idum,idum)
         END IF
         IF ( (ISThkk(i).EQ.In1) .OR. (ISThkk(i).EQ.In2) .OR. 
     &        (ISThkk(i).EQ.In3) .OR. (ISThkk(i).EQ.In4) .OR. 
     &        (ISThkk(i).EQ.In5) ) THEN
            IF ( (Mode.EQ.1) .OR. (Mode.EQ.3) .OR. (Mode.EQ.4) .OR. 
     &           (Mode.EQ.5) ) CALL DT_EVTEMC(-PHKk(1,i),-PHKk(2,i),
     &           -PHKk(3,i),-PHKk(4,i),2,idum,idum)
            IF ( (Mode.EQ.2) .OR. (Mode.EQ.3) .OR. (Mode.EQ.5) )
     &           CALL DT_EVTFLC(IDHkk(i),Mn,-2,idum,idum)
            IF ( (Mode.EQ.4) .OR. (Mode.EQ.5) )
     &           CALL DT_EVTCHG(IDHkk(i),-2,idum,idum)
         END IF
      END DO
      IF ( (Mode.EQ.1) .OR. (Mode.EQ.3) .OR. (Mode.EQ.4) .OR. 
     &     (Mode.EQ.5) ) CALL DT_EVTEMC(dum,dum,dum,dum,5,Ipos,irej1)
      IF ( (Mode.EQ.2) .OR. (Mode.EQ.3) .OR. (Mode.EQ.5) )
     &     CALL DT_EVTFLC(0,idum,3,Ipos,irej2)
      IF ( (Mode.EQ.4) .OR. (Mode.EQ.5) )
     &     CALL DT_EVTCHG(idum,3,Ipos,irej3)
 
      IF ( (irej1.NE.0) .OR. (irej2.NE.0) .OR. (irej3.NE.0) ) Irej = 1
 
      END SUBROUTINE
