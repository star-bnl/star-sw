
      SUBROUTINE PHO_HARXTO(Ecmh,Ptcutr,Ptcutd,Dsigmc,Dsdptc)
C**********************************************************************
C
C     total hard cross section (perturbative QCD, Parton Model)
C
C     input:  ECMH     CMS energy of scattering system
C             PTCUTR   PT cutoff for resolved processes
C             PTCUTD   PT cutoff for direct processes (photon, Pomeron)
C
C     output: DSIGMC(0:MARPR2) cross sections for given cutoff
C             DSDPTC(0:MARPR2) differential cross sections at cutoff
C
C     note:  COMPLEX*16          DSIGMC
C            DOUBLE PRECISION    DSDPTC
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION absz , alphae , Dsdptc , dsigh , dsigl , Ecmh , 
     &                 eec , ex , ex1 , f , f1 , f2 , fac , facc , pt , 
     &                 Ptcutd , Ptcutr , ptmax , ptmin , ptmxx
      DOUBLE PRECISION r , rl , ru , ss , weig , zz
      INTEGER i , k , m , MAX_PRO_2 , npoint
      SAVE 
 
      PARAMETER (MAX_PRO_2=16)
      COMPLEX*16 Dsigmc
      DIMENSION Dsigmc(0:MAX_PRO_2) , Dsdptc(0:MAX_PRO_2)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  some constants
      INCLUDE 'inc/pocons'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
 
      DOUBLE PRECISION PHO_ALPHAE
 
      COMPLEX*16 dsig1
      DIMENSION dsig1(0:MAX_PRO_2)
      DIMENSION absz(32) , weig(32)
 
      DATA fac/3.0D0/
 
      DO m = 0 , MAX_PRO_2
         Dsigmc(m) = DCMPLX(0.D0,0.D0)
      END DO
C  integration up to kin. limit, but not further than PARMDL(96) GeV
      eec = MIN(Ecmh/2.001D0,PARmdl(96))
C
      IF ( Ptcutr.LT.eec ) THEN
C
C  integration for resolved processes
         ptmin = Ptcutr
         ptmax = MIN(fac*ptmin,eec)
         npoint = NGAup1
         CALL PHO_HARXPT(Ecmh,ptmin,1,dsig1)
         DO m = 1 , 9
            Dsdptc(m) = DREAL(dsig1(m))
         END DO
         dsigh = DREAL(dsig1(9))
         ptmxx = 0.95D0*ptmax
         CALL PHO_HARXPT(Ecmh,ptmxx,1,dsig1)
         dsigl = DREAL(dsig1(9))
         ex = LOG(dsigh/(dsigl+1.D-30))/LOG(fac)
         ex1 = 1.0D0 - ex
         DO k = 1 , 2
            IF ( ptmin.LT.ptmax ) THEN
               rl = ptmin**ex1
               ru = ptmax**ex1
               CALL PHO_GAUSET(rl,ru,npoint,absz,weig)
               DO i = 1 , npoint
                  r = absz(i)
                  pt = r**(1.0D0/ex1)
                  CALL PHO_HARXPT(Ecmh,pt,1,dsig1)
                  f = weig(i)*pt/(r*ex1)
                  DO m = 1 , 9
                     Dsigmc(m) = Dsigmc(m) + f*dsig1(m)
                  END DO
               END DO
            END IF
            ptmin = ptmax
            ptmax = eec
            npoint = NGAup2
         END DO
      END IF
      Dsigmc(0) = Dsigmc(9)
      Dsdptc(0) = Dsdptc(9)
C
C  integration for direct processes
      IF ( (Ptcutd.GE.eec) .OR. (Ptcutd.LT.0.5D0) ) RETURN
C
      IF ( (IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990) .OR. (IDPdg2.EQ.22) .OR. 
     &     (IDPdg2.EQ.990) ) THEN
         ptmin = Ptcutd
         ptmax = MIN(fac*ptmin,eec)
         npoint = NGAup1
         CALL PHO_HARXPT(Ecmh,ptmin,2,dsig1)
         IF ( DREAL(dsig1(15)).GE.1.D-15 ) THEN
            DO m = 10 , 16
               Dsdptc(m) = DREAL(dsig1(m))
            END DO
            dsigh = DREAL(dsig1(15)-dsig1(14))
            ptmxx = 0.95D0*ptmax
            CALL PHO_HARXPT(Ecmh,ptmxx,2,dsig1)
            dsigl = DREAL(dsig1(15)-dsig1(14))
            ex = LOG(dsigh/(dsigl+1.D-30))/LOG(fac)
            ex1 = 1.0D0 - ex
            DO k = 1 , 2
               IF ( ptmin.LT.ptmax ) THEN
                  rl = ptmin**ex1
                  ru = ptmax**ex1
                  CALL PHO_GAUSET(rl,ru,npoint,absz,weig)
                  DO i = 1 , npoint
                     r = absz(i)
                     pt = r**(1.0D0/ex1)
                     CALL PHO_HARXPT(Ecmh,pt,2,dsig1)
                     f = weig(i)*pt/(r*ex1)
                     DO m = 10 , 15
                        Dsigmc(m) = Dsigmc(m) + f*dsig1(m)
                     END DO
                  END DO
               END IF
               ptmin = ptmax
               ptmax = eec
               npoint = NGAup2
            END DO
         END IF
      END IF
C
C
C  double direct process
      IF ( ((IDPdg1.EQ.22) .OR. (IDPdg1.EQ.990)) .AND. 
     &     ((IDPdg2.EQ.22) .OR. (IDPdg2.EQ.990)) ) THEN
         facc = 0.D0
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
            facc = facc + f1*f2*3.D0
         END DO
 
         zz = SQRT(1.D0-4.D0*Ptcutd*Ptcutd/ss)
         r = 4.D0*PI/ss*(LOG((1.D0+zz)/(1.D0-zz))-zz)*GEV2mb
C  hadronic cross section
         Dsigmc(14) = r*facc*AKFac
C  leptonic cross section
         IF ( (IDPdg1.EQ.22) .AND. (IDPdg2.EQ.22) ) THEN
            Dsigmc(16) = r*3.D0*alphae**2*AKFac
C  simulation of tau together with quarks
            IF ( IPAmdl(64).NE.0 ) Dsigmc(14) = Dsigmc(14) + Dsigmc(16)
     &           /3.D0
            Dsigmc(16) = Dsigmc(16)*2.D0/3.D0
         ELSE
            Dsigmc(16) = DCMPLX(0.D0,0.D0)
         END IF
C  sum of direct part
         Dsigmc(15) = DCMPLX(0.D0,0.D0)
         DO i = 10 , 14
            Dsigmc(15) = Dsigmc(15) + Dsigmc(i)
         END DO
      END IF
C total sum (hadronic)
      Dsigmc(0) = Dsigmc(9) + Dsigmc(15)
      Dsdptc(0) = Dsdptc(9) + Dsdptc(15)
 
      END SUBROUTINE
