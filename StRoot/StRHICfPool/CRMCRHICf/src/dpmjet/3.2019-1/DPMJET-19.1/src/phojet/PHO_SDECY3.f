
      SUBROUTINE PHO_SDECY3(Umo,Am1,Am2,Am3,Isp)
C**********************************************************************
C
C     isotropic/anisotropic three particle decay in CM system,
C     (transversely/longitudinally polarized boson into three
C     pseudo-scalar mesons)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Am1 , am11 , Am2 , am22 , am23 , Am3 , am33 , 
     &                 cfe , cod32 , costh , costh1 , costh2 , cx11 , 
     &                 cx22 , cy11 , cy22 , cz11 , cz22 , DEPS , ds2
      DOUBLE PRECISION DT_RNDM , EPS , f , fh , go , gu , ofak , 
     &                 PHO_XLAM , rho , rho1 , rho2 , s1 , s2 , s21 , 
     &                 s22 , s2sup , s3 , sfe , sid1 , sid2
      DOUBLE PRECISION sid3 , sid32 , sinth1 , sinth2 , suprho , ufak , 
     &                 Umo , umo2 , umoo , x1 , x4 , x5 , xo , xx , y
      INTEGER i , ia , ii , iii , Isp , ith
      SAVE 
 
      PARAMETER (DEPS=1.D-30,EPS=1.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  auxiliary data for three particle decay
      INCLUDE 'inc/po3dcy'
 
      DIMENSION f(5) , xx(5)
 
C  calculation of maximum of S2 phase space weight
      umoo = Umo + Umo
      gu = (Am2+Am3)**2
      go = (Umo-Am1)**2
      ufak = 1.0000000000001D0
      IF ( gu.GT.go ) ufak = 0.99999999999999D0
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
         rho2 = PHO_XLAM(s22,umo2,am11)*PHO_XLAM(s22,am22,am33)
     &          /(s22+EPS)
         IF ( rho2.LT.rho1 ) GOTO 100
      END DO
 
 100  s2sup = (s22-s21)/2.D0 + s21
      suprho = PHO_XLAM(s2sup,umo2,am11)*PHO_XLAM(s2sup,am22,am33)
     &         /(s2sup+EPS)
      suprho = suprho*1.05D0
      xo = s21 - ds2
      IF ( gu.LT.go .AND. xo.LT.gu ) xo = gu
      IF ( gu.GT.go .AND. xo.GT.gu ) xo = gu
      xx(1) = xo
      xx(3) = s22
      x1 = (xo+s22)*0.5D0
      xx(2) = x1
      f(3) = rho2
      f(1) = PHO_XLAM(xo,umo2,am11)*PHO_XLAM(xo,am22,am33)/(xo+EPS)
      f(2) = PHO_XLAM(x1,umo2,am11)*PHO_XLAM(x1,am22,am33)/(x1+EPS)
      DO i = 1 , 16
         x4 = (xx(1)+xx(2))*0.5D0
         x5 = (xx(2)+xx(3))*0.5D0
         f(4) = PHO_XLAM(x4,umo2,am11)*PHO_XLAM(x4,am22,am33)/(x4+EPS)
         f(5) = PHO_XLAM(x5,umo2,am11)*PHO_XLAM(x5,am22,am33)/(x5+EPS)
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
 
C  selection of S1
      ith = 0
 200  ith = ith + 1
      IF ( ith.GT.200 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I10)')
     &         'PHO_SDECY3:ERROR: too many iterations' , ith
         CALL PHO_ABORT
      END IF
      s2 = am23 + DT_RNDM(Am2)*((Umo-Am1)**2-am23)
      y = DT_RNDM(am23)*suprho
      rho = PHO_XLAM(s2,umo2,am11)*PHO_XLAM(s2,am22,am33)/s2
      IF ( y.GT.rho ) GOTO 200
 
C  selection of S2
      s1 = DT_RNDM(Am2)*rho + am11 + am22 - (s2-umo2+am11)
     &     *(s2+am22-am33)/(2.D0*s2) - rho/2.D0
      s3 = umo2 + am11 + am22 + am33 - s1 - s2
      ECM(1) = (umo2+am11-s2)/umoo
      ECM(2) = (umo2+am22-s3)/umoo
      ECM(3) = (umo2+am33-s1)/umoo
      PCM(1) = SQRT((ECM(1)+Am1)*(ECM(1)-Am1))
      PCM(2) = SQRT((ECM(2)+Am2)*(ECM(2)-Am2))
      PCM(3) = SQRT((ECM(3)+Am3)*(ECM(3)-Am3))
 
C  calculation of angles: TH between p1,p2; TH1 p3,p1; TH2 p3,p2
      IF ( (PCM(1).LE.EPS) .OR. (PCM(2).LE.EPS) ) THEN
         costh = (DT_RNDM(s1)-0.5D0)*2.D0
      ELSE
         costh = (ECM(1)*ECM(2)+0.5D0*(am11+am22-s1))/(PCM(1)*PCM(2))
      END IF
      costh2 = (PCM(3)*PCM(3)+PCM(2)*PCM(2)-PCM(1)*PCM(1))
     &         /(2.D0*PCM(2)*PCM(3))
      sinth2 = SQRT(1.D0-costh2*costh2)
      sinth1 = costh2*SQRT(1.D0-costh*costh) - costh*sinth2
      costh1 = costh*costh2 + sinth2*SQRT(1.D0-costh*costh)
 
C  selection of the sperical coordinates of particle 3
      CALL PHO_SFECFE(SIF(3),COF(3))
      IF ( Isp.EQ.0 ) THEN
C  no polarization
         COD(3) = 2.D0*DT_RNDM(s2) - 1.D0
      ELSE IF ( Isp.EQ.1 ) THEN
C  transverse polarization
 250     COD(3) = 2.D0*DT_RNDM(s1) - 1.D0
         sid32 = 1.D0 - COD(3)*COD(3)
         IF ( sid32.LT.DT_RNDM(costh) ) GOTO 250
      ELSE IF ( Isp.EQ.2 ) THEN
C  longitudinal polarization
 300     COD(3) = 2.D0*DT_RNDM(costh2) - 1.D0
         cod32 = COD(3)*COD(3)
         IF ( cod32.LT.DT_RNDM(sinth1) ) GOTO 300
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I3)')
     &        'PHO_SDECY3:ERROR: ' , 'invalid polarization' , Isp
         CALL PHO_ABORT
      END IF
 
C  selection of the rotation angle of p1-p2 plane along p3
      IF ( Isp.EQ.0 ) THEN
         CALL PHO_SFECFE(sfe,cfe)
      ELSE
         sfe = 0.D0
         cfe = 1.D0
      END IF
      cx11 = -costh1
      cy11 = sinth1*cfe
      cz11 = sinth1*sfe
      cx22 = -costh2
      cy22 = -sinth2*cfe
      cz22 = -sinth2*sfe
 
      sid3 = SQRT((1.D0+COD(3))*(1.D0-COD(3)))
      COD(1) = cx11*COD(3) + cz11*sid3
      IF ( (1.D0-COD(1)*COD(1)).LT.DEPS ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,5E12.4)')
     &         'PHO_SDECY3: COS(TH1) > 1' , COD(1) , COF(3) , sid3 , 
     &        cx11 , cz11
         CALL PHO_PREVNT(-1)
      END IF
 
      sid1 = SQRT((1.D0+COD(1))*(1.D0-COD(1)))
      COF(1) = (cx11*sid3*COF(3)-cy11*SIF(3)-cz11*COD(3)*COF(3))/sid1
      SIF(1) = (cx11*sid3*SIF(3)+cy11*COF(3)-cz11*COD(3)*SIF(3))/sid1
      COD(2) = cx22*COD(3) + cz22*sid3
      sid2 = SQRT((1.D0+COD(2))*(1.D0-COD(2)))
      COF(2) = (cx22*sid3*COF(3)-cy22*SIF(3)-cz22*COD(3)*COF(3))/sid2
      SIF(2) = (cx22*sid3*SIF(3)+cy22*COF(3)-cz22*COD(3)*SIF(3))/sid2
 
      END SUBROUTINE
