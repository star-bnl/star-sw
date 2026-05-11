
      SUBROUTINE DT_EMC1(Pp1,Pp2,Pt1,Pt2,Mode,Ipos,Irej)
 
C***********************************************************************
C This version dated 15.12.94 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION dum , Pp1 , Pp2 , Pt1 , Pt2 , TINY10
      INTEGER idum , Ipos , Irej , irej1 , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10)
 
      DIMENSION Pp1(4) , Pp2(4) , Pt1(4) , Pt2(4)
 
      Irej = 0
 
 
      IF ( (Mode.EQ.0) .OR. (ABS(Mode).GT.3) .AND. LPRi.GT.4 )
     &     WRITE (LOUt,'(1X,A,I6)') 'EMC1: not supported MODE ' , Mode
 
      IF ( (Mode.GT.0) .AND. (Mode.LT.3) ) THEN
         IF ( Mode.EQ.1 ) THEN
            CALL DT_EVTEMC(Pp1(1),Pp1(2),Pp1(3),Pp1(4),1,idum,idum)
         ELSE IF ( Mode.EQ.2 ) THEN
            CALL DT_EVTEMC(Pp1(1),Pp1(2),Pp1(3),Pp1(4),2,idum,idum)
         END IF
         CALL DT_EVTEMC(Pp2(1),Pp2(2),Pp2(3),Pp2(4),2,idum,idum)
         CALL DT_EVTEMC(Pt1(1),Pt1(2),Pt1(3),Pt1(4),2,idum,idum)
         CALL DT_EVTEMC(Pt2(1),Pt2(2),Pt2(3),Pt2(4),2,idum,idum)
      ELSE IF ( Mode.LT.0 ) THEN
         IF ( Mode.EQ.-1 ) THEN
            CALL DT_EVTEMC(-Pp1(1),-Pp1(2),-Pp1(3),-Pp1(4),1,idum,idum)
         ELSE IF ( Mode.EQ.-2 ) THEN
            CALL DT_EVTEMC(-Pp1(1),-Pp1(2),-Pp1(3),-Pp1(4),2,idum,idum)
         END IF
         CALL DT_EVTEMC(-Pp2(1),-Pp2(2),-Pp2(3),-Pp2(4),2,idum,idum)
         CALL DT_EVTEMC(-Pt1(1),-Pt1(2),-Pt1(3),-Pt1(4),2,idum,idum)
         CALL DT_EVTEMC(-Pt2(1),-Pt2(2),-Pt2(3),-Pt2(4),2,idum,idum)
      END IF
 
      IF ( ABS(Mode).EQ.3 ) THEN
         CALL DT_EVTEMC(dum,dum,dum,dum,3,Ipos,irej1)
 
         IF ( irej1.NE.0 ) Irej = 1
      END IF
      END SUBROUTINE
