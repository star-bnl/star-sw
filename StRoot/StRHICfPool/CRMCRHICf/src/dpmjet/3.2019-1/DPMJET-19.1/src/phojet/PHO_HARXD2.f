
      SUBROUTINE PHO_HARXD2(Ecmh,Pt,Etac,Dsigmc)
C**********************************************************************
C
C     differential cross section DSIG/(DETAC*DPT) for direct processes
C
C     input:  ECMH     CMS energy of scattering system
C             PT       parton PT
C             ETAC     pseudorapidity of parton C
C
C     output: DSIGMC(0:15) QCD-PM cross sections dsigma/dpt/detac
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alpha1 , alpha2 , dsigm , ec , Ecmh , ed , EPS , 
     &                 Etac , fac2 , factor , ONEP1 , pda , pdb , Pt , 
     &                 qqal , qqpd , s1 , s2 , s3 , sp
      DOUBLE PRECISION TINY , TINY6 , tp , tt , up , uu , x , xa , xb
      INTEGER i , MAX_PRO_2
      SAVE 
 
      PARAMETER (MAX_PRO_2=16)
      COMPLEX*16 Dsigmc
      DIMENSION Dsigmc(0:MAX_PRO_2)
      PARAMETER (TINY=1.D-30,ONEP1=1.1,TINY6=1.D-06,EPS=1.D-25)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
C  scale parameters for parton model calculations
      INCLUDE 'inc/pohscl'
C  some constants
      INCLUDE 'inc/pocons'
 
      DOUBLE PRECISION PHO_ALPHAS , PHO_ALPHAE
      DIMENSION pda(-6:6) , pdb(-6:6) , dsigm(0:MAX_PRO_2)
 
C     ONE32=1.D0/9.D0
C     TWO32=4.D0/9.D0
      DO i = 10 , 13
         Dsigmc(i) = DCMPLX(0.D0,0.D0)
         dsigm(i) = 0.D0
      END DO
      Dsigmc(15) = DCMPLX(0.D0,0.D0)
      dsigm(15) = 0.D0
 
C  direct particle 1
      IF ( (IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990) ) THEN
         ec = EXP(Etac)
         ed = Ecmh/Pt - ec
C  kinematic conversions
         xa = 1.D0
         xb = 1.D0/(ec*ed)
         IF ( xb.GE.1.D0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &            'PHO_HARXD2:ERROR: XB>1 (XA,XB)' , xa , xb
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
 
         alpha2 = PHO_ALPHAS(qqal,2)
         IF ( IDPdg1.EQ.22 ) THEN
            alpha1 = PHO_ALPHAE(qqal)
         ELSE IF ( IDPdg1.EQ.990 ) THEN
            alpha1 = PARmdl(74)
         END IF
         factor = -PI2*GEV2mb*up/Pt*alpha1*alpha2/sp*AKFac
C  parton distribution (times x)
         CALL PHO_PDF(2,xb,qqpd,0.D0,pdb)
         s1 = pdb(0)
C  charge counting
         s2 = 0.D0
         s3 = 0.D0
         IF ( IDPdg1.EQ.22 ) THEN
            DO i = 1 , NF
C           IF(MOD(I,2).EQ.0) THEN
C             S2 = S2 + (PDB(I)+PDB(-I))*TWO32
C             S3 = S3 + TWO32
C           ELSE
C             S2 = S2 + (PDB(I)+PDB(-I))*ONE32
C             S3 = S3 + ONE32
C           ENDIF
               s2 = s2 + (pdb(i)+pdb(-i))*Q_Ch2(i)
               s3 = s3 + Q_Ch2(i)
            END DO
         ELSE IF ( IDPdg1.EQ.990 ) THEN
            DO i = 1 , NF
               s2 = s2 + pdb(i) + pdb(-i)
            END DO
            s3 = NF
         END IF
C  partial cross sections (including color and symmetry factors)
C  direct photon matrix elements
         dsigm(10) = -8.D0/3.D0*(uu+1.D0)/up
         dsigm(11) = (uu+tt)/(up*tp)
C
         dsigm(10) = factor*dsigm(10)*s2
         dsigm(11) = factor*dsigm(11)*s1*s3
C  complex part
         x = ABS(tp-up)
         fac2 = -LOG((x+2.D0)/(x+1.D-30))/PI
C
         DO i = 10 , 11
            IF ( dsigm(i).LT.0.D0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3,1P,2E12.4)')
     &               'PHO_HARXD2: neg. cross section' , i , dsigm(i) , 
     &              Ecmh
               dsigm(i) = 0.D0
            END IF
            Dsigmc(i) = DCMPLX(dsigm(i),dsigm(i)*fac2)
            Dsigmc(15) = Dsigmc(15) + Dsigmc(i)
         END DO
      END IF
C
C  direct particle 2
      IF ( (IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990) ) THEN
         ec = EXP(Etac)
         ed = 1.D0/(Ecmh/Pt-1.D0/ec)
C  kinematic conversions
         xa = Pt*(ec+ed)/Ecmh
         xb = 1.D0
         IF ( xa.GE.1.D0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.4)')
     &            'PHO_HARXD2:ERROR:XA>1 (XA,XB)' , xa , xb
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
 
         alpha1 = PHO_ALPHAS(qqal,1)
         IF ( IDPdg2.EQ.22 ) THEN
            alpha2 = PHO_ALPHAE(qqal)
         ELSE IF ( IDPdg2.EQ.990 ) THEN
            alpha2 = PARmdl(74)
         END IF
         factor = -PI2*GEV2mb*tp/Pt*alpha1*alpha2/sp*AKFac
C  parton distribution (times x)
         CALL PHO_PDF(1,xa,qqpd,0.D0,pda)
         s1 = pda(0)
C  charge counting
         s2 = 0.D0
         s3 = 0.D0
         IF ( IDPdg2.EQ.22 ) THEN
            DO i = 1 , NF
C           IF(MOD(I,2).EQ.0) THEN
C             S2 = S2 + (PDA(I)+PDA(-I))*TWO32
C             S3 = S3 + TWO32
C           ELSE
C             S2 = S2 + (PDA(I)+PDA(-I))*ONE32
C             S3 = S3 + ONE32
C           ENDIF
               s2 = s2 + (pda(i)+pda(-i))*Q_Ch2(i)
               s3 = s3 + Q_Ch2(i)
            END DO
         ELSE IF ( IDPdg2.EQ.990 ) THEN
            DO i = 1 , NF
               s2 = s2 + pda(i) + pda(-i)
            END DO
            s3 = NF
         END IF
C  partial cross sections (including color and symmetry factors)
C  direct photon matrix elements
         dsigm(12) = -8.D0/3.D0*(tt+1.D0)/tp
         dsigm(13) = (uu+tt)/(up*tp)
C
         dsigm(12) = factor*dsigm(12)*s2
         dsigm(13) = factor*dsigm(13)*s3*s1
C  complex part
         x = ABS(tp-up)
         fac2 = -LOG((x+2.D0)/(x+1.D-30))/PI
C
         DO i = 12 , 13
            IF ( dsigm(i).LT.0.D0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3,1P,2E12.4)')
     &               'PHO_HARXD2: neg. cross section:' , i , dsigm(i) , 
     &              Ecmh
               dsigm(i) = 0.D0
            END IF
            Dsigmc(i) = DCMPLX(dsigm(i),dsigm(i)*fac2)
            Dsigmc(15) = Dsigmc(15) + Dsigmc(i)
         END DO
      END IF
      END SUBROUTINE
