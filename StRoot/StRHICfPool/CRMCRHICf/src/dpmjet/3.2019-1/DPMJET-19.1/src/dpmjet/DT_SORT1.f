
      SUBROUTINE DT_SORT1(A,Idx,N,I0,I1,Mode)
 
C***********************************************************************
C This subroutine sorts entries in A in increasing/decreasing order    *
C of A(i).                                                             *
C              MODE  = 1     increasing in A(i=1..N)                   *
C                    = 2     decreasing in A(i=1..N)                   *
C This version dated 21.04.95 is revised by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION A , b
      INTEGER i , I0 , I1 , Idx , ix , j , l , m , Mode , N
      SAVE 
 
      DIMENSION A(N) , Idx(N)
 
      m = I1
 100  m = I1 - 1
      IF ( m.LE.0 ) RETURN
      l = 0
      DO i = I0 , m
         j = i + 1
         IF ( Mode.EQ.1 ) THEN
            IF ( A(i).LE.A(j) ) GOTO 200
         ELSE IF ( A(i).GE.A(j) ) THEN
            GOTO 200
         END IF
         b = A(i)
         A(i) = A(j)
         A(j) = b
         ix = Idx(i)
         Idx(i) = Idx(j)
         Idx(j) = ix
         l = 1
 200  END DO
      IF ( l.EQ.1 ) GOTO 100
 
      END SUBROUTINE
