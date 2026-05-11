
      SUBROUTINE PHO_HARXPT(Ecmh,Pt,Ipro,Dsigmc)
C**********************************************************************
C
C     differential cross section DSIG/DPT
C
C     input:  ECMH     CMS energy of scattering system
C             PT       parton PT
C             IPRO     1  resolved processes
C                      2  direct processes
C                      3  resolved and direct processes
C
C     output: DSIGMC(0:12) QCD-PM cross sections dsigma/dpt
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION absz , alphae , amt , ecl , Ecmh , ecu , EPS , 
     &                 f1 , f2 , fac , ONEP1 , Pt , ss , t1 , t2 , 
     &                 TINY , weig , xm , zz
      INTEGER i , Ipro , m , MAX_PRO_2 , npoint
      SAVE 
 
      PARAMETER (MAX_PRO_2=16)
      COMPLEX*16 Dsigmc
      DIMENSION Dsigmc(0:MAX_PRO_2)
      PARAMETER (TINY=1.D-10,ONEP1=1.1,EPS=1.D-25)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  some constants
      INCLUDE 'inc/pocons'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
 
      DOUBLE PRECISION PHO_ALPHAE
 
      COMPLEX*16 dsig1
      DIMENSION dsig1(0:MAX_PRO_2)
      DIMENSION absz(32) , weig(32)
 
      DO m = 0 , MAX_PRO_2
         Dsigmc(m) = DCMPLX(0.D0,0.D0)
         dsig1(m) = DCMPLX(0.D0,0.D0)
      END DO
 
C  resolved and direct processes
      amt = 2.D0*Pt/Ecmh
      IF ( amt.GE.1.D0 ) RETURN
      ecu = LOG((SQRT(1.D0-amt*amt)+1.D0)/amt)
      ecl = -ecu
      npoint = NGAuet
      CALL PHO_GAUSET(ecl,ecu,npoint,absz,weig)
      DO i = 1 , npoint
         dsig1(9) = DCMPLX(0.D0,0.D0)
         dsig1(15) = DCMPLX(0.D0,0.D0)
         IF ( Ipro.EQ.1 ) THEN
            CALL PHO_HARXR2(Ecmh,Pt,absz(i),dsig1)
         ELSE IF ( Ipro.EQ.2 ) THEN
            CALL PHO_HARXD2(Ecmh,Pt,absz(i),dsig1)
         ELSE
            CALL PHO_HARXR2(Ecmh,Pt,absz(i),dsig1)
            CALL PHO_HARXD2(Ecmh,Pt,absz(i),dsig1)
         END IF
         DO m = 1 , MAX_PRO_2
            Dsigmc(m) = Dsigmc(m) + weig(i)*dsig1(m)
         END DO
      END DO
 
C  direct processes
      IF ( ((IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990)) .AND. 
     &     ((IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990)) ) THEN
         fac = 0.D0
         ss = Ecmh*Ecmh
         alphae = PHO_ALPHAE(ss)
         DO i = 1 , NF
            IF ( IDPdg1.EQ.22 ) THEN
C           F1 = (4.D0-3.D0*MOD(I,2))/9.D0*ALPHAE
               f1 = Q_Ch2(i)*alphae
            ELSE
               f1 = PARmdl(74)
            END IF
            IF ( IDPdg2.EQ.22 ) THEN
C           F2 = (4.D0-3.D0*MOD(I,2))/9.D0*ALPHAE
               f2 = Q_Ch2(i)*alphae
            ELSE
               f2 = PARmdl(74)
            END IF
            fac = fac + f1*f2*3.D0
         END DO
C  direct cross sections
         zz = SQRT(1.D0-4.D0*Pt*Pt/ss+TINY)
         t1 = -ss/2.D0*(1.D0+zz)
         t2 = -ss/2.D0*(1.D0-zz)
         xm = -2.D0*Pt/zz*((ss+t1)/t1+t1/(ss+t1)+(ss+t2)/t2+t2/(ss+t2))
C  hadronic part
         Dsigmc(14) = GEV2mb*2.D0*PI*fac/(ss*ss)*xm*AKFac
 
C  leptonic part (e, mu, tau)
         Dsigmc(16) = 0.D0
         IF ( (IDPdg1.EQ.22) .AND. (IDPdg2.EQ.22) ) THEN
            Dsigmc(16) = Dsigmc(14)/fac*3.D0*alphae**2
C  simulation of tau together with quarks
            IF ( IPAmdl(64).NE.0 ) Dsigmc(14) = Dsigmc(14) + Dsigmc(16)
     &           /3.D0
         END IF
      END IF
 
      Dsigmc(15) = Dsigmc(15) + Dsigmc(14)
      Dsigmc(0) = Dsigmc(9) + Dsigmc(15)
 
      END SUBROUTINE
