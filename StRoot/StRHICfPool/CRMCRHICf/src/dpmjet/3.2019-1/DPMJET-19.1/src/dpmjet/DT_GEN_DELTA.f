
      SUBROUTINE DT_GEN_DELTA(Enu,Llep,Ltarg,Jint,P21,P22,P23,P24,P25)
 
      IMPLICIT NONE
      DOUBLE PRECISION amd , amd0 , amdmin , aml , aml0 , aml2 , amn , 
     &                 bet , beta1 , beta2 , beta3 , ctstar , deld , 
     &                 dsig , dsigmax , DT_DSIGMA_DELTA , eistar , el , 
     &                 elf , Enu
      DOUBLE PRECISION enuu , et , gam , gamd , P21 , P22 , P23 , P24 , 
     &                 P25 , phi , phi11 , phi12 , pi , pl , plf , plt , 
     &                 plz , po , pstar
      DOUBLE PRECISION PYR , q2 , q2max , q2min , r , s , sqs
      INTEGER is , Jint , k0 , k1 , ka , kw , lbad , ll , Llep , lnu , 
     &        Ltarg , nbad , ntry
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C...Generate a Delta-production neutrino/antineutrino
C.  CC-interaction on a nucleon
C
C.  INPUT  ENU (GeV) = Neutrino Energy
C.         LLEP = neutrino type
C.         LTARG = nucleon target type 1=p, 2=n.
C.         JINT = 1:CC, 2::NC
C.
C.  OUTPUT PPL(4)  4-monentum of final lepton
C----------------------------------------------------
 
      INCLUDE 'inc/pyjets'
 
C*sr - removed (not needed)
C     COMMON /CBAD/  LBAD, NBAD
C*
 
      DIMENSION pi(3) , po(3)
C     REAL*4 AMD0, AMD, AMN(2), AML0(6), AML, AML2, AMDMIN
      DIMENSION aml0(6) , amn(2)
      DATA amd0/1.231D+00/ , gamd/0.12D+00/ , deld/0.169D+00/ , 
     &     amdmin/1.084D+00/
      DATA amn/0.93827231D+00 , 0.93956563D+00/
      DATA aml0/2*0.51100D-03 , 2*0.105659D+00 , 2*1.777D+00/
 
C     WRITE(6,*)' GEN_DEL',ENU,LLEP,LTARG,JINT,P21,P22,P23,P24,P25
      lbad = 0
C...Final lepton mass
      IF ( Jint.EQ.1 ) THEN
         aml = aml0(Llep)
      ELSE
         aml = 0.D+00
      END IF
      aml2 = aml**2
 
C...Particle labels (LUND)
      N = 5
      K(1,1) = 21
      K(2,1) = 21
      K(3,1) = 21
      K(4,1) = 1
      K(3,3) = 1
      K(4,3) = 1
      IF ( Ltarg.EQ.1 ) THEN
         K(2,2) = 2212
      ELSE
         K(2,2) = 2112
      END IF
      k0 = (Llep-1)/2
      k1 = Llep/2
      ka = 12 + 2*k0
      is = -1 + 2*Llep - 4*k1
      lnu = 2 - Llep + 2*k1
      K(1,2) = is*ka
      K(5,1) = 1
      K(5,3) = 2
      IF ( Jint.EQ.1 ) THEN                     ! CC interactions
         K(3,2) = is*24
         K(4,2) = is*(ka-1)
         IF ( lnu.EQ.1 ) THEN
            IF ( Ltarg.EQ.1 ) THEN
               K(5,2) = 2224
            ELSE
               K(5,2) = 2214
            END IF
         ELSE IF ( Ltarg.EQ.1 ) THEN
            K(5,2) = 2114
         ELSE
            K(5,2) = 1114
         END IF
      ELSE
         K(3,2) = 23            ! NC (Z0) interactions
         K(4,2) = K(1,2)
C*sr 7.5.00: swop Delta's (bug), Delta+ for proton (LTARG=1),
C                                Delta0 for neutron (LTARG=2)
C        IF (LTARG .EQ. 1)  THEN
C           K(5,2) = 2114
C        ELSE
C           K(5,2) = 2214
C        ENDIF
         IF ( Ltarg.EQ.1 ) THEN
            K(5,2) = 2214
         ELSE
            K(5,2) = 2114
         END IF
C*
      END IF
 
C...4-momentum initial lepton
      P(1,5) = 0.D+00
      P(1,4) = Enu
      P(1,1) = 0.D+00
      P(1,2) = 0.D+00
      P(1,3) = Enu
C...4-momentum initial nucleon
      P(2,5) = amn(Ltarg)
C     P(2,4) = P(2,5)
C     P(2,1) = 0.
C     P(2,2) = 0.
C     P(2,3) = 0.
      P(2,1) = P21
      P(2,2) = P22
      P(2,3) = P23
      P(2,4) = P24
      P(2,5) = P25
      N = 2
      beta1 = -P(2,1)/P(2,4)
      beta2 = -P(2,2)/P(2,4)
      beta3 = -P(2,3)/P(2,4)
      N = 2
 
      CALL PYROBO(0,0,0.0D0,0.0D0,beta1,beta2,beta3)
 
C     print*,' nucl. rest fram ( fermi incl.) prima della rotazione'
 
      phi11 = ATAN(P(1,2)/P(1,3))
      pi(1) = P(1,1)
      pi(2) = P(1,2)
      pi(3) = P(1,3)
 
      CALL DT_TESTROT(pi,po,phi11,1)
      DO ll = 1 , 3
         IF ( ABS(po(ll)).LT.1.D-07 ) po(ll) = 0.D+00
      END DO
      P(1,1) = po(1)
      P(1,2) = po(2)
      P(1,3) = po(3)
      phi12 = ATAN(P(1,1)/P(1,3))
 
      pi(1) = P(1,1)
      pi(2) = P(1,2)
      pi(3) = P(1,3)
      CALL DT_TESTROT(pi,po,phi12,2)
      DO ll = 1 , 3
         IF ( ABS(po(ll)).LT.1.D-07 ) po(ll) = 0.D+00
      END DO
      P(1,1) = po(1)
      P(1,2) = po(2)
      P(1,3) = po(3)
 
      enuu = P(1,4)
 
C...Generate the Mass of the Delta
      ntry = 0
 100  r = PYR(0)
      amd = amd0 + 0.5D+00*gamd*TAN((2.D+00*r-1.D+00)
     &      *ATAN(2.D+00*deld/gamd))
      ntry = ntry + 1
      IF ( ntry.GT.1000 ) THEN
         lbad = 1
Caf: 1 line, make the compiler happy
         nbad = 0
         WRITE (LOUt,99010) nbad , enuu , amd , amdmin , amd0 , gamd , 
     &                      et
99010    FORMAT (2X,'DT_GEN_DELTA : event rejected ',I5,6G10.3)
         RETURN
      END IF
      IF ( amd.LT.amdmin ) GOTO 100
      et = ((amd+aml)**2-amn(Ltarg)**2)/(2.D+00*amn(Ltarg))
      IF ( enuu.LT.et ) GOTO 100
 
C...Kinematical  limits in Q**2
      s = amn(Ltarg)**2 + 2.D+00*amn(Ltarg)*enuu
      sqs = SQRT(s)
      pstar = (s-amn(Ltarg)**2)/(2.D+00*sqs)
      elf = (s-amd**2+aml2)/(2.D+00*sqs)
      plf = SQRT(elf**2-aml2)
      q2min = -aml2 + 2.D+00*pstar*(elf-plf)
      q2max = -aml2 + 2.D+00*pstar*(elf+plf)
      IF ( q2min.LT.0.D+00 ) q2min = 0.D+00
 
      dsigmax = DT_DSIGMA_DELTA(lnu,-q2min,s,aml,amd)
 200  q2 = q2min + (q2max-q2min)*PYR(0)
      dsig = DT_DSIGMA_DELTA(lnu,-q2,s,aml,amd)
      IF ( dsig.LT.dsigmax*PYR(0) ) GOTO 200
 
C...Generate the kinematics of the final particles
      eistar = (s+amn(Ltarg)**2)/(2.D+00*sqs)
      gam = eistar/amn(Ltarg)
      bet = pstar/eistar
      ctstar = elf/plf - (q2+aml2)/(2.D+00*pstar*plf)
      el = gam*(elf+bet*plf*ctstar)
      plz = gam*(plf*ctstar+bet*elf)
      pl = SQRT(el**2-aml2)
      plt = SQRT(MAX(1.D-06,(pl*pl-plz*plz)))
      phi = 6.28319D+00*PYR(0)
      P(4,1) = plt*COS(phi)
      P(4,2) = plt*SIN(phi)
      P(4,3) = plz
      P(4,4) = el
      P(4,5) = aml
 
C...4-momentum of Delta
      P(5,1) = -P(4,1)
      P(5,2) = -P(4,2)
      P(5,3) = enuu - P(4,3)
      P(5,4) = enuu + amn(Ltarg) - P(4,4)
      P(5,5) = amd
 
C...4-momentum  of intermediate boson
      P(3,5) = -q2
      P(3,4) = P(1,4) - P(4,4)
      P(3,1) = P(1,1) - P(4,1)
      P(3,2) = P(1,2) - P(4,2)
      P(3,3) = P(1,3) - P(4,3)
      N = 5
 
      DO kw = 1 , 5
         pi(1) = P(kw,1)
         pi(2) = P(kw,2)
         pi(3) = P(kw,3)
         CALL DT_TESTROT(pi,po,phi12,3)
         DO ll = 1 , 3
            IF ( ABS(po(ll)).LT.1.D-07 ) po(ll) = 0.D+00
         END DO
         P(kw,1) = po(1)
         P(kw,2) = po(2)
         P(kw,3) = po(3)
      END DO
 
C********************************************
 
      DO kw = 1 , 5
         pi(1) = P(kw,1)
         pi(2) = P(kw,2)
         pi(3) = P(kw,3)
         CALL DT_TESTROT(pi,po,phi11,4)
         DO ll = 1 , 3
            IF ( ABS(po(ll)).LT.1.D-07 ) po(ll) = 0.D+00
         END DO
         P(kw,1) = po(1)
         P(kw,2) = po(2)
         P(kw,3) = po(3)
      END DO
C********************************************
C         transform back into Lab.
 
      CALL PYROBO(0,0,0.0D0,0.0D0,-beta1,-beta2,-beta3)
 
C     WRITE(6,*)' Lab fram ( fermi incl.) '
      N = 5
      CALL PYEXEC
 
      END SUBROUTINE
