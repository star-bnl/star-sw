
      SUBROUTINE DT_DTHREP(Umo,Ecm1,Ecm2,Ecm3,Pcm1,Pcm2,Pcm3,Cod1,Cof1,
     &                     Sif1,Cod2,Cof2,Sif2,Cod3,Cof3,Sif3,Am1,Am2,
     &                     Am3)
 
C***********************************************************************
C Three-particle decay.                                                *
C  UMO                 cm-energy of the decaying system       (input)  *
C  AM1/2/3             masses of the decay products           (input)  *
C  ECM1/2/2,PCM1/2/3   cm-energies/momenta of the decay prod. (output) *
C  COD,COF,SIF         direction cosines of the decay prod.   (output) *
C                                                                      *
C Threpd89: slight revision by A. Ferrari                              *
C Last change on   11-oct-93   by    Alfredo Ferrari, INFN - Milan     *
C Revised by S. Roesler, 20.11.95                                      *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION aam1 , aam2 , aam3 , Am1 , am11 , Am2 , am22 , 
     &                 am23 , Am3 , am33 , ANGLSQ , AZRZRZ , c , cfe , 
     &                 chlp , Cod1 , Cod2 , Cod3 , Cof1 , Cof2
      DOUBLE PRECISION Cof3 , costh , costh1 , costh2 , cx11 , cx22 , 
     &                 cy11 , cy22 , cz11 , cz22 , ds2 , DT_RNDM , 
     &                 DT_YLAMB , Ecm1 , Ecm2 , Ecm3 , eochck , eocmpr , 
     &                 eps , f
      DOUBLE PRECISION fh , go , gu , ofak , ONEMNS , ONEONE , ONEPLS , 
     &                 Pcm1 , pcm12 , Pcm2 , Pcm3 , PIPIPI , pxchck , 
     &                 pychck , pzchck , rho , rho1 , rho2 , s1 , s2
      DOUBLE PRECISION s21 , s22 , s2sup , s3 , sfe , sid1 , sid2 , 
     &                 sid3 , Sif1 , Sif2 , Sif3 , sinth , sinth1 , 
     &                 sinth2 , suprho , TWOTWO , ufak , Umo , umo2 , 
     &                 umoo
      DOUBLE PRECISION uumo , uw , x1 , x4 , x5 , xo , xx , y
      INTEGER i , ia , ii , iii , ith
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ANGLSQ=2.5D-31)
      PARAMETER (AZRZRZ=1.0D-30)
      PARAMETER (ONEMNS=0.999999999999999D+00)
      PARAMETER (ONEPLS=1.000000000000001D+00)
      PARAMETER (ONEONE=1.D+00)
      PARAMETER (TWOTWO=2.D+00)
      PARAMETER (PIPIPI=3.1415926535897932270D+00)
 
      INCLUDE 'inc/hngamr'
C flags for input different options
      INCLUDE 'inc/dtflg1'
 
      DIMENSION f(5) , xx(5)
      DATA eps/AZRZRZ/
 
      umoo = Umo + Umo
C***S1, S2, S3 ARE THE INVARIANT MASSES OF THE PARTICLES 1, 2, 3
C***J. VON NEUMANN - RANDOM - SELECTION OF S2
C***CALCULATION OF THE MAXIMUM OF THE S2 - DISTRIBUTION
      uumo = Umo
      aam1 = Am1
      aam2 = Am2
      aam3 = Am3
      gu = (Am2+Am3)**2
      go = (Umo-Am1)**2
C     UFAK=1.0000000000001D0
C     IF (GU.GT.GO) UFAK=0.9999999999999D0
      IF ( gu.GT.go ) THEN
         ufak = ONEMNS
      ELSE
         ufak = ONEPLS
      END IF
      ofak = 2.D0 - ufak
      gu = gu*ufak
      go = go*ofak
      ds2 = (go-gu)/99.D0
      am11 = Am1*Am1
      am22 = Am2*Am2
      am33 = Am3*Am3
      umo2 = Umo*Umo
      rho2 = 0.D0
      s22 = gu
      DO i = 1 , 100
         s21 = s22
         s22 = gu + (i-1.D0)*ds2
         rho1 = rho2
         rho2 = DT_YLAMB(s22,umo2,am11)*DT_YLAMB(s22,am22,am33)
     &          /(s22+eps)
         IF ( rho2.LT.rho1 ) GOTO 100
      END DO
 100  s2sup = (s22-s21)*.5D0 + s21
      suprho = DT_YLAMB(s2sup,umo2,am11)*DT_YLAMB(s2sup,am22,am33)
     &         /(s2sup+eps)
      suprho = suprho*1.05D0
      xo = s21 - ds2
      IF ( gu.LT.go .AND. xo.LT.gu ) xo = gu
      IF ( gu.GT.go .AND. xo.GT.gu ) xo = gu
      xx(1) = xo
      xx(3) = s22
      x1 = (xo+s22)*0.5D0
      xx(2) = x1
      f(3) = rho2
      f(1) = DT_YLAMB(xo,umo2,am11)*DT_YLAMB(xo,am22,am33)/(xo+eps)
      f(2) = DT_YLAMB(x1,umo2,am11)*DT_YLAMB(x1,am22,am33)/(x1+eps)
      DO i = 1 , 16
         x4 = (xx(1)+xx(2))*0.5D0
         x5 = (xx(2)+xx(3))*0.5D0
         f(4) = DT_YLAMB(x4,umo2,am11)*DT_YLAMB(x4,am22,am33)/(x4+eps)
         f(5) = DT_YLAMB(x5,umo2,am11)*DT_YLAMB(x5,am22,am33)/(x5+eps)
         xx(4) = x4
         xx(5) = x5
         DO ii = 1 , 5
            ia = ii
            DO iii = ia , 5
               IF ( f(ii).LT.f(iii) ) THEN
                  fh = f(ii)
                  f(ii) = f(iii)
                  f(iii) = fh
                  fh = xx(ii)
                  xx(ii) = xx(iii)
                  xx(iii) = fh
               END IF
            END DO
         END DO
         suprho = f(1)
         s2sup = xx(1)
         DO ii = 1 , 3
            ia = ii
            DO iii = ia , 3
               IF ( xx(ii).LT.xx(iii) ) THEN
                  fh = f(ii)
                  f(ii) = f(iii)
                  f(iii) = fh
                  fh = xx(ii)
                  xx(ii) = xx(iii)
                  xx(iii) = fh
               END IF
            END DO
         END DO
      END DO
      am23 = (Am2+Am3)**2
      ith = 0
      REDu = 2.D0
 200  ith = ith + 1
      IF ( ith.GT.200 ) REDu = -9.D0
      IF ( ith.LE.200 ) THEN
         c = DT_RNDM(REDu)
C     S2=AM23+C*((UMO-AM1)**2-AM23)
         s2 = am23 + c*(Umo-Am1-Am2-Am3)*(Umo-Am1+Am2+Am3)
         y = DT_RNDM(s2)
         y = y*suprho
         rho = DT_YLAMB(s2,umo2,am11)*DT_YLAMB(s2,am22,am33)/s2
C***RANDOM SELECTION OF S3 AND CALCULATION OF S1
         IF ( y.GT.rho ) GOTO 200
         s1 = DT_RNDM(s2)
         s1 = s1*rho + am11 + am22 - (s2-umo2+am11)*(s2+am22-am33)
     &        /(2.D0*s2) - rho*.5D0
         s3 = umo2 + am11 + am22 + am33 - s1 - s2
         Ecm1 = (umo2+am11-s2)/umoo
         Ecm2 = (umo2+am22-s3)/umoo
         Ecm3 = (umo2+am33-s1)/umoo
         Pcm1 = SQRT((Ecm1+Am1)*(Ecm1-Am1))
         Pcm2 = SQRT((Ecm2+Am2)*(Ecm2-Am2))
         Pcm3 = SQRT((Ecm3+Am3)*(Ecm3-Am3))
         CALL DT_DSFECF(sfe,cfe)
C***TH IS THE ANGLE BETWEEN PARTICLES 1 AND 2
C***TH1, TH2 ARE THE ANGLES BETWEEN PARTICLES 1, 2 AND THE DIRECTION OF
         pcm12 = Pcm1*Pcm2
         IF ( pcm12.LT.ANGLSQ ) THEN
            uw = DT_RNDM(s1)
            costh = (uw-0.5D+00)*2.D+00
         ELSE
            costh = (Ecm1*Ecm2+0.5D+00*(am11+am22-s1))/pcm12
         END IF
C     IF(ABS(COSTH).GT.0.9999999999999999D0)
C    &COSTH=SIGN(0.9999999999999999D0,COSTH)
         IF ( ABS(costh).GT.ONEONE ) costh = SIGN(ONEONE,costh)
         IF ( REDu.LT.1.D+00 ) RETURN
         costh2 = (Pcm3*Pcm3+Pcm2*Pcm2-Pcm1*Pcm1)/(2.D+00*Pcm2*Pcm3)
C     IF(ABS(COSTH2).GT.0.9999999999999999D0)
C    &COSTH2=SIGN(0.9999999999999999D0,COSTH2)
         IF ( ABS(costh2).GT.ONEONE ) costh2 = SIGN(ONEONE,costh2)
         sinth2 = SQRT((ONEONE-costh2)*(ONEONE+costh2))
         sinth = SQRT((ONEONE-costh)*(ONEONE+costh))
         sinth1 = costh2*sinth - costh*sinth2
         costh1 = costh*costh2 + sinth2*sinth
C***RANDOM SELECTION OF THE SPHERICAL COORDINATES OF THE DIRECTION OF PA
C***CFE, SFE ARE COS AND SIN OF THE ROTATION ANGLE OF THE SYSTEM 1, 2 AR
C***THE DIRECTION OF PARTICLE 3
C***CALCULATION OF THE SPHERICAL COORDINATES OF PARTICLES 1, 2
         cx11 = -costh1
         cy11 = sinth1*cfe
         cz11 = sinth1*sfe
         cx22 = -costh2
         cy22 = -sinth2*cfe
         cz22 = -sinth2*sfe
         CALL DT_DSFECF(Sif3,Cof3)
         Cod3 = TWOTWO*DT_RNDM(cx11) - ONEONE
         sid3 = SQRT((1.D+00-Cod3)*(1.D+00+Cod3))
         Cod1 = cx11*Cod3 + cz11*sid3
         chlp = (ONEONE-Cod1)*(ONEONE+Cod1)
         IF ( chlp.LT.1.D-14 ) WRITE (LOUt,99010) Cod1 , Cof3 , sid3 , 
     &        cx11 , cz11
99010    FORMAT (5F20.15)
         sid1 = SQRT(chlp)
         Cof1 = (cx11*sid3*Cof3-cy11*Sif3-cz11*Cod3*Cof3)/sid1
         Sif1 = (cx11*sid3*Sif3+cy11*Cof3-cz11*Cod3*Sif3)/sid1
         Cod2 = cx22*Cod3 + cz22*sid3
         sid2 = SQRT((ONEONE-Cod2)*(ONEONE+Cod2))
         Cof2 = (cx22*sid3*Cof3-cy22*Sif3-cz22*Cod3*Cof3)/sid2
         Sif2 = (cx22*sid3*Sif3+cy22*Cof3-cz22*Cod3*Sif3)/sid2
      END IF
C === Energy conservation check: === *
      eochck = Umo - Ecm1 - Ecm2 - Ecm3
C     SID1 = SQRT ( ( ONEONE - COD1 ) * ( ONEONE + COD1 ) )
C     SID2 = SQRT ( ( ONEONE - COD2 ) * ( ONEONE + COD2 ) )
C     SID3 = SQRT ( ( ONEONE - COD3 ) * ( ONEONE + COD3 ) )
      pzchck = Pcm1*Cod1 + Pcm2*Cod2 + Pcm3*Cod3
      pxchck = Pcm1*Cof1*sid1 + Pcm2*Cof2*sid2 + Pcm3*Cof3*sid3
      pychck = Pcm1*Sif1*sid1 + Pcm2*Sif2*sid2 + Pcm3*Sif3*sid3
      eocmpr = 1.D-12*Umo
      IF ( ABS(eochck)+ABS(pxchck)+ABS(pychck)+ABS(pzchck).GT.eocmpr )
     &     THEN
C*sr 5.5.95 output-unit changed
         IF ( IOUlev(1).GT.0 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) 
     &          ' *** Threpd: energy/momentum conservation failure! ***'
     &          , eochck , pxchck , pychck , pzchck
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,*) ' *** SID1,SID2,SID3' , 
     &           sid1 , sid2 , sid3
         END IF
C*
      END IF
      END SUBROUTINE
