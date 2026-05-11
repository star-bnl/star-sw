
      DOUBLE PRECISION FUNCTION PHO_ALLM97(Q2,W)
C**********************************************************************
C
C     ALLM97 parametrization for gamma*-p cross section
C     (for F2 see comments, code adapted from V. Shekelyan, H1)
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      DOUBLE PRECISION Q2 , W
      DOUBLE PRECISION m02 , m12 , lam2 , m22
      DOUBLE PRECISION s11 , s12 , s13 , a11 , a12 , a13 , b11 , b12 , 
     &                 b13
      DOUBLE PRECISION s21 , s22 , s23 , a21 , a22 , a23 , b21 , b22 , 
     &                 b23
      DOUBLE PRECISION alfa , xmp2 , w2 , q02 , s , t , t0 , z , cin , 
     &                 ap , bp , ar , br , xp , xr , sr , sp , f2p , f2r
      DATA alfa , xmp2/112.2D0 , .8802D0/
 
      w2 = W*W
      PHO_ALLM97 = 0.D0
 
C  pomeron
      s11 = 0.28067D0
      s12 = 0.22291D0
      s13 = 2.1979D0
      a11 = -0.0808D0
      a12 = -0.44812D0
      a13 = 1.1709D0
      b11 = 0.60243D0
      b12 = 1.3754D0
      b13 = 1.8439D0
      m12 = 49.457D0
 
C  reggeon
      s21 = 0.80107D0
      s22 = 0.97307D0
      s23 = 3.4942D0
      a21 = 0.58400D0
      a22 = 0.37888D0
      a23 = 2.6063D0
      b21 = 0.10711D0
      b22 = 1.9386D0
      b23 = 0.49338D0
      m22 = 0.15052D0
C
      m02 = 0.31985D0
      lam2 = 0.065270D0
      q02 = 0.46017D0 + lam2
 
C
      s = 0.
      t = LOG((Q2+q02)/lam2)
      t0 = LOG(q02/lam2)
      IF ( Q2.GT.0.D0 ) s = LOG(t/t0)
      z = 1.D0
 
      IF ( Q2.GT.0.D0 ) z = (w2-xmp2)/(Q2+w2-xmp2)
 
      IF ( s.LT.0.01D0 ) THEN
 
C   pomeron part
 
         xp = 1.D0/(1.D0+(w2-xmp2)/(Q2+m12))
 
         ap = a11
         bp = b11**2
 
         sp = s11
         f2p = sp*xp**ap*z**bp
 
C   reggeon part
 
         xr = 1.D0/(1.D0+(w2-xmp2)/(Q2+m22))
 
         ar = a21
         br = b21**2
 
         sr = s21
         f2r = sr*xr**ar*z**br
 
      ELSE
 
C   pomeron part
 
         xp = 1.D0/(1.D0+(w2-xmp2)/(Q2+m12))
 
         ap = a11 + (a11-a12)*(1.D0/(1.D0+s**a13)-1.D0)
 
         bp = b11**2 + b12**2*s**b13
 
         sp = s11 + (s11-s12)*(1.D0/(1.D0+s**s13)-1.D0)
 
         f2p = sp*xp**ap*z**bp
 
C   reggeon part
 
         xr = 1.D0/(1.D0+(w2-xmp2)/(Q2+m22))
 
         ar = a21 + a22*s**a23
         br = b21**2 + b22**2*s**b23
 
         sr = s21 + s22*s**s23
         f2r = sr*xr**ar*z**br
 
      END IF
 
C     F2 = (F2P+F2R)*Q2/(Q2+M02)
 
      cin = alfa/(Q2+m02)*(1.D0+4.D0*xmp2*Q2/(Q2+w2-xmp2)**2)/z
      PHO_ALLM97 = cin*(f2p+f2r)
 
      END FUNCTION
