
      SUBROUTINE PHO_HARWGI(Ecmx,Ptcut,Nkon,Z,Fdis)
C**********************************************************************
C
C     auxiliary subroutine to find maximum of remaining weight
C
C     input:  ECMX   current CMS energy
C             PTCUT  current pt cutoff
C             NKON   process label  1..5  resolved
C                                   6..7  direct particle 1
C                                   8..9  direct particle 2
C                                   10    double direct
C             Z(3)   transformed variable
C
C     output: remaining weight
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alpha1 , alpha2 , ch1 , ch2 , Ecmx , f , Fdis , 
     &                 pda , pdb , Ptcut , ssr , t , TINY , TINY6 , y1 , 
     &                 y2 , Z
      INTEGER i , n , NKM , Nkon
      SAVE 
 
      DIMENSION Z(3)
 
      PARAMETER (NKM=10)
      PARAMETER (TINY=1.D-30,TINY6=1.D-06)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
C  scale parameters for parton model calculations
      INCLUDE 'inc/pohscl'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
 
      DOUBLE PRECISION PHO_ALPHAS , PHO_ALPHAE
      DIMENSION f(NKM) , pda(-6:6) , pdb(-6:6)
 
      Fdis = 0.D0
 
      IF ( LPRi.GT.4 .AND. IDEb(64).GE.25 )
     &      WRITE (LO,'(1X,A,/5X,5E12.3,I5)')
     &     'PHO_HARWGI: ECM,PT,Z,NK' , Ecmx , Ptcut , Z , Nkon
C  check input values
      IF ( Z(1).LT.0.D0 .OR. Z(1).GT.1.D0 ) RETURN
      IF ( Z(2).LT.0.D0 .OR. Z(2).GT.1.D0 ) RETURN
      IF ( Z(3).LT.0.D0 .OR. Z(3).GT.1.D0 ) RETURN
C  transformations
      y1 = EXP(ALNh*Z(1))
      IF ( Nkon.LE.5 ) THEN
C  resolved kinematic
         y2 = -(1.D0-y1) + 2.D0*(1.D0-y1)*Z(2)
         X1 = 0.5D0*(y2+SQRT(y2*y2+4.D0*y1))
         X2 = X1 - y2
         X1 = MIN(X1,0.999999999999D0)
         X2 = MIN(X2,0.999999999999D0)
      ELSE IF ( Nkon.LE.7 ) THEN
C  direct kinematic 1
         X1 = 1.D0
         X2 = MIN(y1,0.999999999999D0)
      ELSE IF ( Nkon.LE.9 ) THEN
C  direct kinematic 2
         X1 = MIN(y1,0.999999999999D0)
         X2 = 1.D0
      ELSE
C  double direct kinematic
         X1 = 1.D0
         X2 = 1.D0
      END IF
      W = SQRT(MAX(TINY,1.D0-AH/y1))
      V = -0.5D0 + W*(Z(3)-0.5D0)
      U = -(1.D0+V)
      PT = MAX(SQRT(U*V*y1*Ecmx*Ecmx),Ptcut)
 
C  set hard scale  QQ  for alpha and partondistr.
      IF ( NQQal.EQ.1 ) THEN
         QQAl = AQQal*PT*PT
      ELSE IF ( NQQal.EQ.2 ) THEN
         QQAl = AQQal*y1*Ecmx*Ecmx*U*V/(1.+V*V+U*U)
      ELSE IF ( NQQal.EQ.3 ) THEN
         QQAl = AQQal*y1*Ecmx*Ecmx
      ELSE IF ( NQQal.EQ.4 ) THEN
         QQAl = AQQal*y1*Ecmx*Ecmx*(U*V)**(1./3.)
      END IF
      IF ( NQQpd.EQ.1 ) THEN
         QQPd = AQQpd*PT*PT
      ELSE IF ( NQQpd.EQ.2 ) THEN
         QQPd = AQQpd*y1*Ecmx*Ecmx*U*V/(1.+V*V+U*U)
      ELSE IF ( NQQpd.EQ.3 ) THEN
         QQPd = AQQpd*y1*Ecmx*Ecmx
      ELSE IF ( NQQpd.EQ.4 ) THEN
         QQPd = AQQpd*y1*Ecmx*Ecmx*(U*V)**(1./3.)
      END IF
C
      IF ( Nkon.LE.5 ) THEN
         DO n = 1 , 5
            f(n) = 0.D0
         END DO
C  resolved processes
         alpha1 = PHO_ALPHAS(QQAl,3)
         alpha2 = alpha1
         CALL PHO_PDF(1,X1,QQPd,0.D0,pda)
         CALL PHO_PDF(2,X2,QQPd,0.D0,pdb)
C  calculate full distribution FDIS
         DO i = 1 , NF
            f(2) = f(2) + pda(i)*pdb(-i) + pda(-i)*pdb(i)
            f(3) = f(3) + pda(i)*pdb(i) + pda(-i)*pdb(-i)
            f(4) = f(4) + pda(i) + pda(-i)
            f(5) = f(5) + pdb(i) + pdb(-i)
         END DO
         f(1) = pda(0)*pdb(0)
         t = pda(0)*f(5) + pdb(0)*f(4)
         f(5) = f(4)*f(5) - (f(2)+f(3))
         f(4) = t
      ELSE IF ( Nkon.LE.7 ) THEN
C  direct processes particle 1
         IF ( IDPdg1.EQ.22 ) THEN
            alpha1 = PHO_ALPHAE(QQAl)
            ch1 = 4.D0/9.D0
            ch2 = 3.D0/9.D0
         ELSE IF ( IDPdg1.EQ.990 ) THEN
            alpha1 = PARmdl(74)
            ch1 = 1.D0
            ch2 = 0.D0
         ELSE
            Fdis = -1.D0
            RETURN
         END IF
         alpha2 = PHO_ALPHAS(QQAl,2)
         CALL PHO_PDF(2,X2,QQPd,0.D0,pdb)
         f(6) = 0.D0
         DO i = 1 , NF
            f(6) = f(6) + (pdb(i)+pdb(-i))*(ch1-ch2*MOD(i,2))
         END DO
         f(7) = pdb(0)
      ELSE IF ( Nkon.LE.9 ) THEN
C  direct processes particle 2
         alpha1 = PHO_ALPHAS(QQAl,1)
         IF ( IDPdg2.EQ.22 ) THEN
            alpha2 = PHO_ALPHAE(QQAl)
            ch1 = 4.D0/9.D0
            ch2 = 3.D0/9.D0
         ELSE IF ( IDPdg2.EQ.990 ) THEN
            alpha2 = PARmdl(74)
            ch1 = 1.D0
            ch2 = 0.D0
         ELSE
            Fdis = -1.D0
            RETURN
         END IF
         CALL PHO_PDF(1,X1,QQPd,0.D0,pda)
         f(8) = 0.D0
         DO i = 1 , NF
            f(8) = f(8) + (pda(i)+pda(-i))*(ch1-ch2*MOD(i,2))
         END DO
         f(9) = pda(0)
      ELSE
C  double direct process
         ssr = Ecmx*Ecmx
         IF ( IDPdg1.EQ.22 ) THEN
            alpha1 = PHO_ALPHAE(ssr)
         ELSE IF ( IDPdg1.EQ.990 ) THEN
            alpha1 = PARmdl(74)
         ELSE
            Fdis = -1.D0
            RETURN
         END IF
         IF ( IDPdg2.EQ.22 ) THEN
            alpha2 = PHO_ALPHAE(ssr)
         ELSE IF ( IDPdg2.EQ.990 ) THEN
            alpha2 = PARmdl(74)
         ELSE
            Fdis = -1.D0
            RETURN
         END IF
         f(10) = 1.D0
      END IF
 
      Fdis = MAX(0.D0,f(Nkon)*alpha1*alpha2)
 
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(64).GE.20 )
     &      WRITE (LO,'(1X,A,/2X,I3,2I6,7E11.3)')
     &      'PHO_HARWGI: NKON,ID1,ID2,AL1,AL2,X1,X2,PT,F(NKON),FDIS' , 
     &     Nkon , IDPdg1 , IDPdg2 , alpha1 , alpha2 , X1 , X2 , PT , 
     &     f(Nkon) , Fdis
 
      END SUBROUTINE
