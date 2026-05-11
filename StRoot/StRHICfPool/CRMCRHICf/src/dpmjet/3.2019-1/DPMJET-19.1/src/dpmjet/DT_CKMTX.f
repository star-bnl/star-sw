
      SUBROUTINE DT_CKMTX(Ipar,X,Scale2,Pd,F2)
      IMPLICIT NONE
      INTEGER Ipar
      REAL owlam , owlam2 , q02 , q1s , q2 , qq , sb , xx
C**********************************************************************
C
C     PDF based on Regge theory, evolved with .... by ....
C
C     input: IPAR     2212   proton (not installed)
C                       45   Pomeron
C                      100   Deuteron
C
C     output: PD(-6:6) x*f(x)  parton distribution functions
C            (PDFLIB convention: d = PD(1), u = PD(2) )
C
C**********************************************************************
 
      SAVE 
      DOUBLE PRECISION X , Scale2 , Pd(-6:6) , cdn , cup , F2
 
      INCLUDE 'inc/dtflka'
 
      DIMENSION qq(7)
C
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
         CALL DT_CKMTPR(1,0,xx,sb,qq(1))
         CALL DT_CKMTPR(2,0,xx,sb,qq(2))
         CALL DT_CKMTPR(3,0,xx,sb,qq(3))
         CALL DT_CKMTPR(4,0,xx,sb,qq(4))
         CALL DT_CKMTPR(5,0,xx,sb,qq(5))
         CALL DT_CKMTPR(8,0,xx,sb,qq(6))
         CALL DT_CKMTPR(7,0,xx,sb,qq(7))
C     ELSEIF (IPAR.EQ.45) THEN
C       CALL CKMTPO(1,0,XX,SB,QQ(1))
C       CALL CKMTPO(2,0,XX,SB,QQ(2))
C       CALL CKMTPO(3,0,XX,SB,QQ(3))
C       CALL CKMTPO(4,0,XX,SB,QQ(4))
C       CALL CKMTPO(5,0,XX,SB,QQ(5))
C       CALL CKMTPO(8,0,XX,SB,QQ(6))
C       CALL CKMTPO(7,0,XX,SB,QQ(7))
      ELSE IF ( Ipar.EQ.100 ) THEN
         CALL DT_CKMTDE(1,0,xx,sb,qq(1))
         CALL DT_CKMTDE(2,0,xx,sb,qq(2))
         CALL DT_CKMTDE(3,0,xx,sb,qq(3))
         CALL DT_CKMTDE(4,0,xx,sb,qq(4))
         CALL DT_CKMTDE(5,0,xx,sb,qq(5))
         CALL DT_CKMTDE(8,0,xx,sb,qq(6))
         CALL DT_CKMTDE(7,0,xx,sb,qq(7))
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,I4,A)') 'CKMTX:   IPAR =' , 
     &        Ipar , ' not implemented!'
         STOP
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
      IF ( Ipar.EQ.45 ) THEN
         cdn = (Pd(1)-Pd(-1))/2.D0
         cup = (Pd(2)-Pd(-2))/2.D0
         Pd(-1) = Pd(-1) + cdn
         Pd(-2) = Pd(-2) + cup
         Pd(1) = Pd(-1)
         Pd(2) = Pd(-2)
      END IF
      F2 = 4.0D0/9.0D0*(Pd(2)-Pd(3)+2.0D0*Pd(3))
     &     + 1.0D0/9.0D0*(Pd(1)-Pd(3)+2.0D0*Pd(3))
     &     + 1.0D0/9.0D0*(2.0D0*Pd(3)) + 4.0D0/9.0D0*(2.0D0*Pd(4))
      END SUBROUTINE
