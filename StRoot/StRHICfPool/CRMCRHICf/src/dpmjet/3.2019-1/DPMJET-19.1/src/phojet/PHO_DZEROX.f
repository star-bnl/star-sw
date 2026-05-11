
      DOUBLE PRECISION FUNCTION PHO_DZEROX(A0,B0,Eps,Maxf,F,Mode)
C**********************************************************************
C
C     Based on
C
C        J.C.P. Bus and T.J. Dekker, Two Efficient Algorithms with
C        Guaranteed Convergence for Finding a Zero of a Function,
C        ACM Trans. Math. Software 1 (1975) 330-345.
C
C        (MODE = 1: Algorithm M;    MODE = 2: Algorithm R)
C
C        CERNLIB C200
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION a , A0 , atl , b , B0 , c , d , Eps , F , fa , 
     &                 fb , fc , fd , fda , fdb , h , HALF , hb , p , q
      DOUBLE PRECISION tol , w , Z1
      INTEGER ie , im1 , im2 , Maxf , mf , Mode
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      CHARACTER NAME*(*)
      PARAMETER (NAME='PHO_DZEROX')
      LOGICAL lmt
      DIMENSION im1(2) , im2(2) , lmt(2)
      EXTERNAL F
 
      PARAMETER (Z1=1,HALF=Z1/2)
 
      DATA im1/2 , 3/ , im2/ - 1 , 3/
 
      IF ( Mode.NE.1 .AND. Mode.NE.2 ) THEN
         c = -2D+10
         IF ( LPRi.GT.4 ) WRITE (LO,99010) NAME , Mode
99010    FORMAT (1X,A,': mode = ',I3,' illegal')
         GOTO 400
      END IF
      fa = F(B0)
      fb = F(A0)
      IF ( fa*fb.GT.0 ) THEN
         c = -3D+10
         IF ( LPRi.GT.4 ) WRITE (LO,99020) NAME
99020    FORMAT (1X,A,': F(A) and F(B) have the same sign')
         GOTO 400
      END IF
      atl = ABS(Eps)
      b = A0
      a = B0
      lmt(2) = .TRUE.
      mf = 2
 100  c = a
      fc = fa
 200  ie = 0
 300  IF ( ABS(fc).LT.ABS(fb) ) THEN
         IF ( c.NE.a ) THEN
            d = a
            fd = fa
         END IF
         a = b
         b = c
         c = a
         fa = fb
         fb = fc
         fc = fa
      END IF
      tol = atl*(1+ABS(c))
      h = HALF*(c+b)
      hb = h - b
      IF ( ABS(hb).GT.tol ) THEN
         IF ( ie.GT.im1(Mode) ) THEN
            w = hb
         ELSE
            tol = tol*SIGN(Z1,hb)
            p = (b-a)*fb
            lmt(1) = ie.LE.1
            IF ( lmt(Mode) ) THEN
               q = fa - fb
               lmt(2) = .FALSE.
            ELSE
               fdb = (fd-fb)/(d-b)
               fda = (fd-fa)/(d-a)
               p = fda*p
               q = fdb*fa - fda*fb
            END IF
            IF ( p.LT.0 ) THEN
               p = -p
               q = -q
            END IF
            IF ( ie.EQ.im2(Mode) ) p = p + p
            IF ( p.EQ.0 .OR. p.LE.q*tol ) THEN
               w = tol
            ELSE IF ( p.LT.hb*q ) THEN
               w = p/q
            ELSE
               w = hb
            END IF
         END IF
         d = a
         a = b
         fd = fa
         fa = fb
         b = b + w
         mf = mf + 1
         IF ( mf.GT.Maxf ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,99030) NAME
99030       FORMAT (1X,A,': too many function calls')
            GOTO 400
         END IF
         fb = F(b)
         IF ( fb.EQ.0 .OR. SIGN(Z1,fc).EQ.SIGN(Z1,fb) ) GOTO 100
         IF ( w.EQ.hb ) GOTO 200
         ie = ie + 1
         GOTO 300
      END IF
 400  PHO_DZEROX = c
 
      END FUNCTION
