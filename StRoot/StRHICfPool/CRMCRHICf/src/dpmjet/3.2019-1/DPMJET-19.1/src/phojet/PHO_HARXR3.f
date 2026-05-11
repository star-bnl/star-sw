
      SUBROUTINE PHO_HARXR3(Ecmh,Pt,Etac,Etad,Dsigmc)
C**********************************************************************
C
C     differential cross section DSIG/(DETAC*DETAD*DPT)
C
C     input:  ECMH     CMS energy
C             PT       parton PT
C             ETAC     pseudorapidity of parton C
C             ETAD     pseudorapidity of parton D
C
C     output: DSIGMC(0:15) QCD-PM cross sections dsigma/dpt/detac/detad
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alpha , dsigm , ec , Ecmh , ed , EPS , Etac , 
     &                 Etad , fac2 , factor , ONEP1 , pda , pdb , Pt , 
     &                 qqal , qqpd , s1 , s2 , s3 , s4
      DOUBLE PRECISION s5 , sp , TINY , TINY6 , tp , tt , up , uu , x , 
     &                 xa , xb
      INTEGER i , MAX_PRO_2
      SAVE 
 
      PARAMETER (TINY=1.D-30,ONEP1=1.1,TINY6=1.D-06,EPS=1.D-20)
 
      PARAMETER (MAX_PRO_2=16)
      COMPLEX*16 Dsigmc
      DIMENSION Dsigmc(0:MAX_PRO_2)
      DIMENSION dsigm(0:MAX_PRO_2)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  some constants
      INCLUDE 'inc/pocons'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  scale parameters for parton model calculations
      INCLUDE 'inc/pohscl'
 
      DOUBLE PRECISION PHO_ALPHAS
      DIMENSION pda(-6:6) , pdb(-6:6)
 
      DO i = 1 , 9
         Dsigmc(i) = DCMPLX(0.D0,0.D0)
         dsigm(i) = 0.D0
      END DO
 
      ec = EXP(Etac)
      ed = EXP(Etad)
C  kinematic conversions
      xa = Pt*(ec+ed)/Ecmh
      xb = xa/(ec*ed)
      IF ( (xa.GE.1.D0) .OR. (xb.GE.1.D0) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &         'PHO_HARXR3:ERROR:X1 OR X2 > 1' , xa , xb
         RETURN
      END IF
      sp = xa*xb*Ecmh*Ecmh
      up = -Ecmh*Pt*ec*xb
      up = up/sp
      tp = -(1.D0+up)
      uu = up*up
      tt = tp*tp
C  set hard scale  QQ  for alpha and partondistr.
      IF ( NQQal.EQ.1 ) THEN
         qqal = AQQal*Pt*Pt
      ELSE IF ( NQQal.EQ.2 ) THEN
         qqal = AQQal*sp*up*tp/(1.D0+tt+uu)
      ELSE IF ( NQQal.EQ.3 ) THEN
         qqal = AQQal*sp
      ELSE IF ( NQQal.EQ.4 ) THEN
         qqal = AQQal*sp*(up*tp)**(1.D0/3.D0)
      END IF
      IF ( NQQpd.EQ.1 ) THEN
         qqpd = AQQpd*Pt*Pt
      ELSE IF ( NQQpd.EQ.2 ) THEN
         qqpd = AQQpd*sp*up*tp/(1.D0+tt+uu)
      ELSE IF ( NQQpd.EQ.3 ) THEN
         qqpd = AQQpd*sp
      ELSE IF ( NQQpd.EQ.4 ) THEN
         qqpd = AQQpd*sp*(up*tp)**(1.D0/3.D0)
      END IF
 
      alpha = PHO_ALPHAS(qqal,3)
      factor = PI2*GEV2mb*Pt*(alpha/sp)**2*AKFac
C  parton distributions (times x)
      CALL PHO_PDF(1,xa,qqpd,0.D0,pda)
      CALL PHO_PDF(2,xb,qqpd,0.D0,pdb)
      s1 = pda(0)*pdb(0)
      s2 = 0.D0
      s3 = 0.D0
      s4 = 0.D0
      s5 = 0.D0
      DO i = 1 , NF
         s2 = s2 + pda(i)*pdb(-i) + pda(-i)*pdb(i)
         s3 = s3 + pda(i)*pdb(i) + pda(-i)*pdb(-i)
         s4 = s4 + pda(i) + pda(-i)
         s5 = s5 + pdb(i) + pdb(-i)
      END DO
C  partial cross sections (including color and symmetry factors)
C  resolved photon matrix elements (light quarks)
      dsigm(1) = 2.25D0*(3.-((up*tp)+up/tt+tp/uu))
      dsigm(6) = (4.D0/9.D0)*(uu+tt)
      dsigm(8) = (4.D0/9.D0)*(1.D0+uu)/tt
      dsigm(2) = (16.D0/27.D0)*(uu+tt)/(up*tp) - 3.D0*dsigm(6)
      dsigm(3) = ((1.D0+uu)/tt) - (4.D0/9.D0)*(1.D0+uu)/up
      dsigm(4) = (9.D0/32.D0)*dsigm(2)
      dsigm(5) = dsigm(6) + dsigm(8) - (8.D0/27.D0)*uu/tp
      dsigm(7) = 0.5D0*(dsigm(8)+(4.D0/9.D0)*(1.D0+tt)/uu-(8.D0/27.D0)
     &           /(up*tp))
C
      dsigm(1) = factor*dsigm(1)*s1
      dsigm(2) = factor*dsigm(2)*s2
      dsigm(3) = factor*dsigm(3)*(pda(0)*s5+pdb(0)*s4)
      dsigm(4) = factor*dsigm(4)*s1*NF
      dsigm(5) = factor*dsigm(5)*s2
      dsigm(6) = factor*dsigm(6)*s2*MAX(0,(NF-1))
      dsigm(7) = factor*dsigm(7)*s3
      dsigm(8) = factor*dsigm(8)*(s4*s5-(s2+s3))
C  complex part
      x = ABS(tp-up)
      fac2 = -LOG((x+2.D0)/(x+1.D-30))/PI
C
      DO i = 1 , 8
         IF ( dsigm(i).LT.EPS ) dsigm(i) = 0.D0
         Dsigmc(i) = DCMPLX(dsigm(i),dsigm(i)*fac2)
         Dsigmc(9) = Dsigmc(9) + Dsigmc(i)
      END DO
      END SUBROUTINE
