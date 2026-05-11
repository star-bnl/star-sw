
      SUBROUTINE DT_SORT(A,N,I0,I1,Mode)
 
C***********************************************************************
C This subroutine sorts entries in A in increasing/decreasing order    *
C of A(3,i).                                                           *
C              MODE  = 1     increasing in A(3,i=1..N)                 *
C                    = 2     decreasing in A(3,i=1..N)                 *
C This version dated 21.04.95 is revised by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION A , b , c , d
      INTEGER i , I0 , I1 , j , l , m , Mode , N
      SAVE 
 
      DIMENSION A(3,N)
 
      m = I1
 100  m = I1 - 1
      IF ( m.LE.0 ) RETURN
      l = 0
      DO i = I0 , m
         j = i + 1
         IF ( Mode.EQ.1 ) THEN
            IF ( A(3,i).LE.A(3,j) ) GOTO 200
         ELSE IF ( A(3,i).GE.A(3,j) ) THEN
            GOTO 200
         END IF
         b = A(3,i)
         c = A(1,i)
         d = A(2,i)
         A(3,i) = A(3,j)
         A(2,i) = A(2,j)
         A(1,i) = A(1,j)
         A(3,j) = b
         A(1,j) = c
         A(2,j) = d
         l = 1
 200  END DO
      IF ( l.EQ.1 ) GOTO 100
 
      END SUBROUTINE
