
      SUBROUTINE PHO_HARXR2(Ecmh,Pt,Etac,Dsigmc)
C**********************************************************************
C
C     differential cross section DSIG/(DETAC*DPT)
C
C     input:  ECMH     CMS energy
C             PT       parton PT
C             ETAC     pseudorapidity of parton C
C
C     output: DSIGMC(0:15) QCD-PM cross sections dsigma/dpt/detac
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION absz , arg , ec , Ecmh , edl , edu , Etac , 
     &                 pctrl , Pt , TINY , weig
      INTEGER i , m , MAX_PRO_2 , npoint
      SAVE 
 
      PARAMETER (TINY=1.D-20)
 
      PARAMETER (MAX_PRO_2=16)
      COMPLEX*16 Dsigmc
      DIMENSION Dsigmc(0:MAX_PRO_2)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
 
      COMPLEX*16 dsig1
      DIMENSION dsig1(0:MAX_PRO_2)
      DIMENSION absz(32) , weig(32)
 
      DO m = 1 , 9
         Dsigmc(m) = DCMPLX(0.D0,0.D0)
         dsig1(m) = 0.D0
      END DO
C
      ec = EXP(Etac)
      arg = Ecmh/Pt
      IF ( arg.LE.ec .OR. arg.LE.1.D0/ec ) RETURN
      edu = LOG(arg-ec)
      edl = -LOG(arg-1.D0/ec)
      npoint = NGAuet
      CALL PHO_GAUSET(edl,edu,npoint,absz,weig)
      DO i = 1 , npoint
         CALL PHO_HARXR3(Ecmh,Pt,Etac,absz(i),dsig1)
         DO m = 1 , 9
            pctrl = DREAL(dsig1(m))/TINY
            IF ( pctrl.GE.1.D0 ) Dsigmc(m) = Dsigmc(m) + weig(i)
     &           *dsig1(m)
         END DO
      END DO
      END SUBROUTINE
