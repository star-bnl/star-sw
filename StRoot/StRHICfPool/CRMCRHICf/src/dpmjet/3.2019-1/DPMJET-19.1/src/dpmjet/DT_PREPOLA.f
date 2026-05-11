
      SUBROUTINE DT_PREPOLA(Q2,Jtyp,Enu)
 
      IMPLICIT NONE
      DOUBLE PRECISION a1 , a2 , aa , axial2 , bb , cc , csi , ee , 
     &                 Enu , etb , etl , etn , fa , fa0 , factk , 
     &                 factp , ffa , ffv1 , ffv2 , fm2
      DOUBLE PRECISION fp , frac , fv1 , fv2 , gve , gvm , oldq2 , 
     &                 omega1 , omega2 , omega3 , omega4 , omega4p , 
     &                 omega5 , pxb , pxl , pxn , pyb , pyl , pyn
      DOUBLE PRECISION pzb , pzl , pzn , Q2 , qm2 , rm , rml , rmm , 
     &                 rmpi , ss , su , u , ww1 , ww2 , ww3 , ww4 , 
     &                 ww5 , x , xa
      DOUBLE PRECISION xdc , ydc , zdc
      INTEGER i , Jtyp , ndc
      SAVE 
C
C By G. Battistoni and E. Scapparone (sept. 1997)
C According to:
C     Albright & Jarlskog, Nucl Phys B84 (1975) 467
C
C
 
      INCLUDE 'inc/pyjets'
 
      INCLUDE 'inc/qnpol'
C particle masses used in qel neutrino scattering modules
      INCLUDE 'inc/qnmass'
C steering flags for qel neutrino scattering modules
      INCLUDE 'inc/qneuto'
C*sr - removed (not needed)
C     COMMON /CAXIAL/ FA0, AXIAL2
C     COMMON /TAUTAU/Q(4,5),ETL,PXL,PYL,PZL,
C    &        ETB,PXB,PYB,PZB,ETN,PXN,PYN,PZN
C*
      REAL*8 pol(4,4) , bb2(3)
      DIMENSION ss(6)
C     DATA C0 /0.17590D0 /  ! G_F**2 cos(theta_c)**2 M**2 /(8 pi) 10**-38 cm+2
      DATA ss/1.D0 , -1.D0 , 1.D0 , -1.D0 , 1.D0 , -1.D0/
C*sr uncommented since common block CAXIAL is now commented
      DATA axial2/1.03D0/   ! to be checked
C*
 
      rml = P(4,5)
      rmm = 0.93960D+00
      fm2 = rmm**2
      rmpi = 0.135D+00
      oldq2 = Q2
      fa0 = -1.253D+00
      csi = 3.71D+00                      !
      gve = 1.D0/(1.D0+Q2/(0.84D+00)**2)**2      ! G_e(q**2)
      gvm = (1.D0+csi)*gve           ! G_m (q**2)
      x = Q2/(EMN*EMN)     ! emn=massa barione
      xa = x/4.D0
      fv1 = 1.D0/(1.D0+xa)*(gve+xa*gvm)
      fv2 = 1.D0/(1.D0+xa)*(gvm-gve)
      fa = fa0/(1.D0+Q2/axial2**2)**2
      ffa = fa*fa
      ffv1 = fv1*fv1
      ffv2 = fv2*fv2
      fp = 2.D0*fa*rmm/(rmpi**2+Q2)
      rm = EMLsq(Jtyp)/(EMN*EMN)            ! emlsq(jtyp)
      a1 = (4.D0+x)*ffa - (4.D0-x)*ffv1 + x*ffv2*(1.D0-xa)
     &     + 4.D0*x*fv1*fv2
      a2 = -rm*((fv1+fv2)**2+ffa)
      aa = (xa+0.25D+00*rm)*(a1+a2)
      bb = -x*fa*(fv1+fv2)
      cc = 0.25D+00*(ffa+ffv1+xa*ffv2)
      su = (4.D+00*Enu*EMN-Q2-EMLsq(Jtyp))/(EMN*EMN)
 
      omega1 = ffa + xa*(ffa+(fv1+fv2)**2) ! articolo di ll...-smith
      omega2 = 4.D+00*cc
      omega3 = 2.D+00*fa*(fv1+fv2)
      omega4p = (-(fv1+fv2)**2-(fa+2*fp)**2+(4.0D+00+(Q2/fm2))*fp**2)
      omega5 = omega2
      omega4 = (omega4p-omega2+2*omega5)/4.D+00
      ww1 = 2.D+00*omega1*EMN**2
      ww2 = 2.D+00*omega2*EMN**2
      ww3 = 2.D+00*omega3*EMN**2
      ww4 = 2.D+00*omega4*EMN**2
      ww5 = 2.D+00*omega5*EMN**2
 
      DO i = 1 , 3
         bb2(i) = -P(4,i)/P(4,4)
      END DO
C      WRITE(*,*)
C      WRITE(*,*)
C      WRITE(*,*) 'Prepola: ready to transform to lepton rest frame'
      N = 5
 
      CALL PYROBO(0,0,0.0D0,0.0D0,bb2(1),bb2(2),bb2(3))
 
C NOW PARTICLES ARE IN THE SCATTERED LEPTON  REST FRAME
C      WRITE(*,*)
C      WRITE(*,*)
C      WRITE(*,*) 'Prepola: now in lepton rest frame'
      ee = Enu
      qm2 = Q2 + rml**2
      u = Q2/(2.*rmm)
      frac = qm2*ww1 + (2.D+00*ee*(ee-u)-0.5D+00*qm2)*ww2 - ss(Jtyp)
     &       *(0.5D+00/(rmm**2))*(2.D+00*rmm*ee*Q2-u*qm2)
     &       *ww3 + ((rml**2)/(2.D+00*fm2))*(qm2*ww4-2.D+00*rmm*ee*ww5)
                                                               !<=FM2 inv di RMM!!
 
      factk = 2.D+00*ww1 - ww2 - ss(Jtyp)*(ee/rmm)*ww3 + ((ee-u)/rmm)
     &        *ww5 - ((rml**2)/fm2)*ww4                !<=FM2 inv di RMM!!
 
      factp = 2.D+00*ee/rmm*ww2 - (qm2/(2.D+00*rmm**2))
     &        *(ss(Jtyp)*ww3+ww5)
 
      DO i = 1 , 3
         pol(4,i) = rml*ss(Jtyp)*(factk*P(1,i)+factp*P(2,i))/frac
         POLarx(i) = pol(4,i)
      END DO
 
      PMOdul = 0.D0
      DO i = 1 , 3
         PMOdul = PMOdul + pol(4,i)**2
      END DO
 
      IF ( Jtyp.GT.4 .AND. NEUdec.GT.0 ) THEN
C
C     Tau has decayed in muon
C
         IF ( NEUdec.EQ.1 ) CALL DT_LEPDCYP(EML(Jtyp),EML(Jtyp-2),
     &        POLarx(3),etl,pxl,pyl,pzl,etb,pxb,pyb,pzb,etn,pxn,pyn,pzn)
C
C     Tau has decayed in electron
C
         IF ( NEUdec.EQ.2 ) CALL DT_LEPDCYP(EML(Jtyp),EML(Jtyp-4),
     &        POLarx(3),etl,pxl,pyl,pzl,etb,pxb,pyb,pzb,etn,pxn,pyn,pzn)
         K(4,1) = 15
         K(4,4) = 6
         K(4,5) = 8
         N = N + 3
C
C     fill common for muon(electron)
C
         P(6,1) = pxl
         P(6,2) = pyl
         P(6,3) = pzl
         P(6,4) = etl
         K(6,1) = 1
         IF ( Jtyp.EQ.5 ) THEN
            IF ( NEUdec.EQ.1 ) THEN
               P(6,5) = EML(Jtyp-2)
               K(6,2) = 13
            ELSE IF ( NEUdec.EQ.2 ) THEN
               P(6,5) = EML(Jtyp-4)
               K(6,2) = 11
            END IF
         ELSE IF ( Jtyp.EQ.6 ) THEN
            IF ( NEUdec.EQ.1 ) THEN
               K(6,2) = -13
            ELSE IF ( NEUdec.EQ.2 ) THEN
               K(6,2) = -11
            END IF
         END IF
         K(6,3) = 4
         K(6,4) = 0
         K(6,5) = 0
C
C     fill common for tau_(anti)neutrino
C
         P(7,1) = pxb
         P(7,2) = pyb
         P(7,3) = pzb
         P(7,4) = etb
         P(7,5) = 0.
         K(7,1) = 1
         IF ( Jtyp.EQ.5 ) THEN
            K(7,2) = 16
         ELSE IF ( Jtyp.EQ.6 ) THEN
            K(7,2) = -16
         END IF
         K(7,3) = 4
         K(7,4) = 0
         K(7,5) = 0
C
C     Fill common for muon(electron)_(anti)neutrino
C
         P(8,1) = pxn
         P(8,2) = pyn
         P(8,3) = pzn
         P(8,4) = etn
         P(8,5) = 0.
         K(8,1) = 1
         IF ( Jtyp.EQ.5 ) THEN
            IF ( NEUdec.EQ.1 ) THEN
               K(8,2) = -14
            ELSE IF ( NEUdec.EQ.2 ) THEN
               K(8,2) = -12
            END IF
         ELSE IF ( Jtyp.EQ.6 ) THEN
            IF ( NEUdec.EQ.1 ) THEN
               K(8,2) = 14
            ELSE IF ( NEUdec.EQ.2 ) THEN
               K(8,2) = 12
            END IF
         END IF
         K(8,3) = 4
         K(8,4) = 0
         K(8,5) = 0
      END IF
C      WRITE(*,*)
C      WRITE(*,*)
 
C      IF(PMODUL.GE.1.D+00) THEN
C        WRITE(*,*) 'Pol',(POLARX(I),I=1,3)
C        write(*,*) pmodul
C        DO I=1,3
C          POL(4,I)=POL(4,I)/PMODUL
C          POLARX(I)=POL(4,I)
C        END DO
C        PMODUL=0.
C        DO I=1,3
C          PMODUL=PMODUL+POL(4,I)**2
C        END DO
C        WRITE(*,*) 'Pol',(POLARX(I),I=1,3)
C
C      ENDIF
 
C      WRITE(*,*) 'PMODUL = ',PMODUL
 
C      WRITE(*,*)
C      WRITE(*,*)
C      WRITE(*,*) 'prepola: Now back to nucl rest frame'
 
      CALL PYROBO(1,5,0.0D0,0.0D0,-bb2(1),-bb2(2),-bb2(3))
 
      xdc = V(4,1) + V(4,5)*P(4,1)/P(4,5)
      ydc = V(4,2) + V(4,5)*P(4,2)/P(4,5)
      zdc = V(4,3) + V(4,5)*P(4,3)/P(4,5)
      DO ndc = 6 , 8
         V(ndc,1) = xdc
         V(ndc,2) = ydc
         V(ndc,3) = zdc
      END DO
 
      END SUBROUTINE
