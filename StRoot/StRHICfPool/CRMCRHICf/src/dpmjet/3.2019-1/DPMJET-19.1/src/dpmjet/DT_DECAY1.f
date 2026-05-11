
      SUBROUTINE DT_DECAY1
 
C***********************************************************************
C Decay of resonances stored in DTEVT1.                                *
C This version dated 20.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , idhad , IDT_IPDGHA , idxin , idxout , irej , k , n , 
     &        nend , nsec
      DOUBLE PRECISION pin , pout
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
 
      DIMENSION pin(4) , pout(20,4) , idxout(20)
 
      nend = NHKk
C     DO 1 I=NPOINT(5),NEND
      DO i = NPOint(4) , nend
         IF ( ABS(ISThkk(i)).EQ.1 ) THEN
            DO k = 1 , 4
               pin(k) = PHKk(k,i)
            END DO
            idxin = IDBam(i)
            CALL DT_DECAYS(pin,idxin,pout,idxout,nsec,irej)
            IF ( nsec.GT.1 ) THEN
               DO n = 1 , nsec
                  idhad = IDT_IPDGHA(idxout(n))
                  CALL DT_EVTPUT(1,idhad,i,0,pout(n,1),pout(n,2),
     &               pout(n,3),pout(n,4),0,0,0)
               END DO
            END IF
         END IF
      END DO
 
      END SUBROUTINE
