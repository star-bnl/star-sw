
      SUBROUTINE PHO_CKMTPD(Ipar,X,Scale2,Pd)
      IMPLICIT NONE
      INTEGER Ipar
      REAL owlam , owlam2 , q02 , q1s , q2 , qq , sb , xx
C**********************************************************************
C
C     PDF based on Regge theory, evolved with .... by ....
C
C     input: IPAR     2212   proton (not installed)
C                      990   Pomeron
C
C     output: PD(-6:6) x*f(x)  parton distribution functions
C            (PDFLIB convention: d = PD(1), u = PD(2) )
C
C**********************************************************************
      SAVE 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      DOUBLE PRECISION X , Scale2 , Pd(-6:6) , cdn , cup
      DIMENSION qq(7)
 
      q2 = SNGL(Scale2)
      q1s = q2
      xx = SNGL(X)
C  QCD lambda for evolution
      owlam = 0.23
      owlam2 = owlam**2
C  Q0**2 for evolution
      q02 = 2.D0
C
C
C  the conventions are : q(1)=x*u, q(2)=x*d, q(3)=q(4)=x*sbar=x*ubar=...
C                        q(6)=x*charm, q(7)=x*gluon
C
      sb = 0.
      IF ( q2.GT.q02 ) sb = LOG(LOG(q2/owlam2)/LOG(q02/owlam2))
      IF ( Ipar.EQ.2212 ) THEN
C       CALL PHO_CKMTPR(XX,SB,QQ
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I6)')
     &         'PHO_CKMTPD:ERROR: invalid particle' , Ipar
         CALL PHO_ABORT
      ELSE
         CALL PHO_CKMTPO(xx,sb,qq)
      END IF
C
      Pd(-6) = 0.D0
      Pd(-5) = 0.D0
      Pd(-4) = DBLE(qq(6))
      Pd(-3) = DBLE(qq(3))
      Pd(-2) = DBLE(qq(4))
      Pd(-1) = DBLE(qq(5))
      Pd(0) = DBLE(qq(7))
      Pd(1) = DBLE(qq(2))
      Pd(2) = DBLE(qq(1))
      Pd(3) = DBLE(qq(3))
      Pd(4) = DBLE(qq(6))
      Pd(5) = 0.D0
      Pd(6) = 0.D0
      IF ( Ipar.EQ.990 ) THEN
         cdn = (Pd(1)-Pd(-1))/2.D0
         cup = (Pd(2)-Pd(-2))/2.D0
         Pd(-1) = Pd(-1) + cdn
         Pd(-2) = Pd(-2) + cup
         Pd(1) = Pd(-1)
         Pd(2) = Pd(-2)
      END IF
      END SUBROUTINE
