
      SUBROUTINE DT_CONUCL(X,N,R,Mode)
 
C***********************************************************************
C Calculation of coordinates of nucleons within nuclei.                *
C        X(3,N)   spatial coordinates of nucleons (in fm)  (output)    *
C        N / R    number of nucleons / radius of nucleus   (input)     *
C        MODE = 0 coordinates not sorted                               *
C             = 1 coordinates sorted with increasing X(3,i)            *
C             = 2 coordinates sorted with decreasing X(3,i)            *
C This version dated 26.10.95 is revised by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , i0 , i1 , icsrt , idxsrt , isrt , j , k , k1 , Mode , 
     &        N , NSRT
      DOUBLE PRECISION ONE , ONETHI , R , SQRTWO , THREE , TWO , TWOPI , 
     &                 X , xtmp , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,THREE=3.0D0,
     &           ONETHI=ONE/THREE,SQRTWO=1.414213562D0)
 
      PARAMETER (TWOPI=6.283185307179586454D+00)
 
      PARAMETER (NSRT=10)
      DIMENSION idxsrt(NSRT,200) , icsrt(NSRT)
      DIMENSION X(3,N) , xtmp(3,260)
 
      CALL DT_COORDI(xtmp,idxsrt,icsrt,N,R)
 
      IF ( (Mode.NE.0) .AND. (N.GT.4) ) THEN
         k = 0
         DO i = 1 , NSRT
            IF ( Mode.EQ.2 ) THEN
               isrt = NSRT + 1 - i
            ELSE
               isrt = i
            END IF
            k1 = k
            DO j = 1 , icsrt(isrt)
               k = k + 1
               X(1,k) = xtmp(1,idxsrt(isrt,j))
               X(2,k) = xtmp(2,idxsrt(isrt,j))
               X(3,k) = xtmp(3,idxsrt(isrt,j))
            END DO
            IF ( icsrt(isrt).GT.1 ) THEN
               i0 = k1 + 1
               i1 = k
               CALL DT_SORT(X,N,i0,i1,Mode)
            END IF
         END DO
      ELSE IF ( (Mode.NE.0) .AND. (N.GE.2) .AND. (N.LE.4) ) THEN
         DO i = 1 , N
            X(1,i) = xtmp(1,i)
            X(2,i) = xtmp(2,i)
            X(3,i) = xtmp(3,i)
         END DO
         CALL DT_SORT(X,N,1,N,Mode)
      ELSE
         DO i = 1 , N
            X(1,i) = xtmp(1,i)
            X(2,i) = xtmp(2,i)
            X(3,i) = xtmp(3,i)
         END DO
      END IF
 
      END SUBROUTINE
