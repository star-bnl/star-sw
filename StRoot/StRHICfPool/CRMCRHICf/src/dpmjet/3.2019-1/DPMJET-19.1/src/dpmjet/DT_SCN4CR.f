
      SUBROUTINE DT_SCN4CR(Nch,Idxch,Nchmin,Mode)
 
C***********************************************************************
C SCan q-aq chains for Color Ropes.                                    *
C This version dated 11.01.95 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , Idxch , idxjn , idxmo , idxmo1 , irej1 , j , Mode , 
     &        Nch , Nchmin , nj , njoin
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
 
      DIMENSION Idxch(248) , idxjn(248)
 
      DO i = 1 , Nch
         IF ( Idxch(i).GT.0 ) THEN
            njoin = 1
            idxmo = JMOhkk(1,JMOhkk(1,JMOhkk(Mode,Idxch(i))))
            idxjn(njoin) = i
            IF ( i.LT.Nch ) THEN
               DO j = i + 1 , Nch
                  IF ( Idxch(j).GT.0 ) THEN
                     idxmo1 = JMOhkk(1,JMOhkk(1,JMOhkk(Mode,Idxch(j))))
                     IF ( idxmo.EQ.idxmo1 ) THEN
                        njoin = njoin + 1
                        idxjn(njoin) = j
                     END IF
                  END IF
               END DO
            END IF
            IF ( njoin.GE.Nchmin+2 ) THEN
               nj = INT(DBLE(njoin-Nchmin)/2.0D0)
               DO j = 1 , 2*nj , 2
                  CALL DT_JOIN(Idxch(idxjn(j)),Idxch(idxjn(j+1)),irej1)
                  IF ( irej1.EQ.0 ) THEN
                     Idxch(idxjn(j)) = 0
                     Idxch(idxjn(j+1)) = 0
                  END IF
               END DO
            END IF
         END IF
      END DO
 
      END SUBROUTINE
