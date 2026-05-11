
      SUBROUTINE DT_GEN_QEL(Enu,Ltyp,P21,P22,P23,P24,P25)
 
C...Generate a quasi-elastic   neutrino/antineutrino
C.  Interaction on a nuclear target
C.  INPUT  : LTYP = neutrino type (1,...,6)
C.           ENU (GeV) = neutrino energy
C----------------------------------------------------
 
      IMPLICIT NONE
      DOUBLE PRECISION amf , amf2 , ami , ami2 , aml , aml0 , aml2 , 
     &                 amn , beta1 , beta2 , beta3 , bgx , bgy , bgz , 
     &                 ctstar , dbeta , dbetb , detot , dsig , dsigev
      DOUBLE PRECISION dsigmax , DT_DSQEL_Q2 , ebind , efmax , elf , 
     &                 Enu , enu0 , enucl , enwell , ga , P21 , P22 , 
     &                 P23 , P24 , P25 , pfermi , phi , phi11 , phi12
      DOUBLE PRECISION pi , plf , pnucl , po , pstar , pstart , PYR , 
     &                 q2 , q2max , q2min , s , sqs , ststar
      INTEGER igb , ininu , inipri , is , k0 , k1 , ka , kw , lbad , 
     &        ll , lnu , Ltyp , nbad , ntry
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      INCLUDE 'inc/pyjets'
 
C nuclear potential
      INCLUDE 'inc/dtnpot'
C steering flags for qel neutrino scattering modules
      INCLUDE 'inc/qneuto'
C*sr - removed (not needed)
C     COMMON /CBAD/  LBAD, NBAD
C     COMMON /CNUC/ XMN,XMN2,PFERMI,EFERMI,EBIND,EB2,C0
C*
 
      DIMENSION pi(3) , po(3)
CJR+
      DATA ininu/0/
CJR-
C     REAL*8 DBETA(3)
C     REAL*8 MN(2), ML0(6), ML, ML2, MI, MI2, MF, MF2
      DIMENSION dbeta(3) , dbetb(3) , amn(2) , aml0(6)
      DATA amn/0.93827231D0 , 0.93956563D0/
      DATA aml0/2*0.51100D-03 , 2*0.105659D0 , 2*1.777D0/
      DATA inipri/0/
 
C     DATA PFERMI/0.22D0/
CGB+...Binding Energy
      DATA ebind/0.008D0/
CGB-...
 
      ininu = ininu + 1
      IF ( ininu.EQ.1 ) NDSig = 0
      lbad = 0
      enu0 = Enu
C      write(*,*) enu0
C...Lepton mass
      aml = aml0(Ltyp)       !  massa leptoni
      aml2 = aml**2          !  massa leptoni **2
C...Particle labels (LUND)
      N = 5
      K(1,1) = 21
      K(2,1) = 21
      K(3,1) = 21
      K(3,3) = 1
      K(4,1) = 1
      K(4,3) = 1
      K(5,1) = 1
      K(5,3) = 2
      k0 = (Ltyp-1)/2          !  2
      k1 = Ltyp/2              !  2
      ka = 12 + 2*k0           !  16
      is = -1 + 2*Ltyp - 4*k1  !  -1 +10 -8 = 1
      K(1,2) = is*ka
      K(4,2) = is*(ka-1)
      K(3,2) = is*24
      lnu = 2 - Ltyp + 2*k1    !  2 - 5 + 2 = - 1
      IF ( lnu.EQ.2 ) THEN
         K(2,2) = 2212
         K(5,2) = 2112
         ami = amn(1)
         amf = amn(2)
CJR+
         pfermi = PFErmn(2)
CJR-
      ELSE
         K(2,2) = 2112
         K(5,2) = 2212
         ami = amn(2)
         amf = amn(1)
CJR+
         pfermi = PFErmp(2)
CJR-
      END IF
      ami2 = ami**2
      amf2 = amf**2
 
      DO igb = 1 , 5
         P(3,igb) = 0.
         P(4,igb) = 0.
         P(5,igb) = 0.
      END DO
 
      ntry = 0
CGB+...
      efmax = SQRT(pfermi**2+ami2) - ami               ! max. Fermi Energy
      enwell = efmax + ebind ! depth of nuclear potential well
CGB-...
 
 
C...4-momentum initial lepton
 100  P(1,5) = 0.     ! massa
      P(1,4) = enu0    ! energia
      P(1,1) = 0.     ! px
      P(1,2) = 0.     ! py
      P(1,3) = enu0    ! pz
 
C     PF = PFERMI*PYR(0)**(1./3.)
C       write(23,*) PYR(0)
C      write(*,*) 'Pfermi=',PF
C      PF = 0.
      ntry = ntry + 1
C     IF(ntry.GT.2) WRITE(*,*) ntry,enu0,k2
      IF ( ntry.GT.500 ) THEN
         lbad = 1
Caf: 1 line, make the compiler happy
         nbad = 0
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) nbad , Enu
 
99010    FORMAT (2X,'DT_GEN_QEL   : event rejected ',I5,G10.3)
         RETURN
      END IF
C     CT = -1. + 2.*PYR(0)
C      CT = -1.
C     ST =  SQRT(1.-CT*CT)
C     F = 2.*3.1415926*PYR(0)
C      F = 0.
 
C     P(2,4) = SQRT(PF*PF + MI2) - EBIND  ! energia
C     P(2,1) = PF*ST*COS(F)               ! px
C     P(2,2) = PF*ST*SIN(F)               ! py
C     P(2,3) = PF*CT                      ! pz
C     P(2,5) = SQRT(P(2,4)**2-PF*PF)      ! massa
      P(2,1) = P21
      P(2,2) = P22
      P(2,3) = P23
      P(2,4) = P24
      P(2,5) = P25
      beta1 = -P(2,1)/P(2,4)
      beta2 = -P(2,2)/P(2,4)
      beta3 = -P(2,3)/P(2,4)
      N = 2
C      WRITE(6,*)' before transforming into target rest frame'
 
      CALL PYROBO(0,0,0.0D0,0.0D0,beta1,beta2,beta3)
 
C      print*,' nucl. rest fram ( fermi incl.) prima della rotazione'
      N = 5
 
      phi11 = ATAN(P(1,2)/P(1,3))
      pi(1) = P(1,1)
      pi(2) = P(1,2)
      pi(3) = P(1,3)
 
      CALL DT_TESTROT(pi,po,phi11,1)
      DO ll = 1 , 3
         IF ( ABS(po(ll)).LT.1.D-07 ) po(ll) = 0.
      END DO
C        WRITE(*,*) po
      P(1,1) = po(1)
      P(1,2) = po(2)
      P(1,3) = po(3)
      phi12 = ATAN(P(1,1)/P(1,3))
 
      pi(1) = P(1,1)
      pi(2) = P(1,2)
      pi(3) = P(1,3)
      CALL DT_TESTROT(pi,po,phi12,2)
      DO ll = 1 , 3
         IF ( ABS(po(ll)).LT.1.D-07 ) po(ll) = 0.
      END DO
C        WRITE(*,*) po
      P(1,1) = po(1)
      P(1,2) = po(2)
      P(1,3) = po(3)
 
      Enu = P(1,4)
 
C...Kinematical limits in Q**2
C      S = P(2,5)**2 + 2.*ENU*(P(2,4)-P(2,3)) !            ????
      s = P(2,5)**2 + 2.*Enu*P(2,5)
      sqs = SQRT(s)                          ! E centro massa
      IF ( sqs.LT.(aml+amf+3.E-03) ) GOTO 100
      elf = (s-amf2+aml2)/(2.*sqs)           ! energia leptone finale p
      pstar = (s-P(2,5)**2)/(2.*sqs)       ! p* neutrino nel c.m.
      plf = SQRT(elf**2-aml2)               ! 3-momento leptone finale
      q2min = -aml2 + 2.*pstar*(elf-plf)    ! + o -
      q2max = -aml2 + 2.*pstar*(elf+plf)    ! according con cos(theta)
      IF ( q2min.LT.0. ) q2min = 0.        ! ??? non fisico
 
C...Generate Q**2
      dsigmax = DT_DSQEL_Q2(Ltyp,Enu,q2min)
 200  q2 = q2min + (q2max-q2min)*PYR(0)
      dsig = DT_DSQEL_Q2(Ltyp,Enu,q2)
      IF ( dsig.LT.dsigmax*PYR(0) ) GOTO 200
      CALL DT_QGAUS(q2min,q2max,dsigev,Enu,Ltyp)
      NDSig = NDSig + 1
C     WRITE(6,*)' Q2,Q2min,Q2MAX,DSIGEV',
C    &Q2,Q2min,Q2MAX,DSIGEV
 
C...c.m. frame. Neutrino along z axis
      detot = (P(1,4)) + (P(2,4)) ! e totale
      dbeta(1) = ((P(1,1))+(P(2,1)))/detot   ! px1+px2/etot = beta_x
      dbeta(2) = ((P(1,2))+(P(2,2)))/detot   !
      dbeta(3) = ((P(1,3))+(P(2,3)))/detot   !
C      WRITE(*,*)
C      WRITE(*,*)
C      WRITE(*,*) 'Input values laboratory frame'
      N = 2
 
      CALL PYROBO(0,0,0.0D0,0.0D0,-dbeta(1),-dbeta(2),-dbeta(3))
 
      N = 5
C      STHETA = ULANGL(P(1,3),P(1,1))
C      write(*,*) 'stheta' ,stheta
C      stheta=0.
C      CALL PYROBO (0,0,-STHETA,0.,0.D0,0.D0,0.D0)
C      WRITE(*,*)
C      WRITE(*,*)
C      WRITE(*,*) 'Output values cm frame'
C...Kinematic in c.m. frame
      ctstar = elf/plf - (q2+aml2)/(2.*pstar*plf)   ! cos(theta) cm
      ststar = SQRT(1.-ctstar**2)
      phi = 6.28319*PYR(0) ! random phi tra 0 e 2*pi
      P(4,5) = aml                  ! massa leptone
      P(4,4) = elf                 ! e leptone
      P(4,3) = plf*ctstar          ! px
      P(4,1) = plf*ststar*COS(phi) ! py
      P(4,2) = plf*ststar*SIN(phi) ! pz
 
      P(5,5) = amf                  ! barione
      P(5,4) = (s+amf2-aml2)/(2.*sqs)
                                     ! e barione
      P(5,3) = -P(4,3)             ! px
      P(5,1) = -P(4,1)             ! py
      P(5,2) = -P(4,2)             ! pz
 
      P(3,5) = -q2
      P(3,1) = P(1,1) - P(4,1)
      P(3,2) = P(1,2) - P(4,2)
      P(3,3) = P(1,3) - P(4,3)
      P(3,4) = P(1,4) - P(4,4)
 
C...Transform back to laboratory  frame
C      WRITE(*,*) 'before going back to nucl rest frame'
C      CALL PYROBO (0,0,STHETA,0.,0.D0,0.D0,0.D0)
      N = 5
 
      CALL PYROBO(0,0,0.0D0,0.0D0,dbeta(1),dbeta(2),dbeta(3))
 
C      WRITE(*,*) 'Now back in nucl rest frame'
 
C********************************************
 
      IF ( Ltyp.GE.3 ) CALL DT_PREPOLA(q2,Ltyp,Enu)
      DO kw = 1 , 5
         pi(1) = P(kw,1)
         pi(2) = P(kw,2)
         pi(3) = P(kw,3)
         CALL DT_TESTROT(pi,po,phi12,3)
         DO ll = 1 , 3
            IF ( ABS(po(ll)).LT.1.D-07 ) po(ll) = 0.
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
            IF ( ABS(po(ll)).LT.1.D-07 ) po(ll) = 0.
         END DO
         P(kw,1) = po(1)
         P(kw,2) = po(2)
         P(kw,3) = po(3)
      END DO
 
C********************************************
 
C      WRITE(*,*) 'Now back in lab frame'
 
      CALL PYROBO(1,5,0.0D0,0.0D0,-beta1,-beta2,-beta3)
 
CGB+...
C...test (on final momentum of nucleon) if Fermi-blocking
C...is operating
      enucl = SQRT(P(5,1)**2+P(5,2)**2+P(5,3)**2+P(5,5)**2) - P(5,5)
      IF ( enucl.LT.efmax ) THEN
C         WRITE(6,*)' qel: Pauli ENUCL.LT.EFMAX ', ENUCL,EFMAX
C...the interaction is not possible due to Pauli-Blocking and
C...it must be resampled
         IF ( inipri.LT.10 ) inipri = inipri + 1
         GOTO 100
      ELSE IF ( enucl.LT.enwell .AND. enucl.GE.efmax ) THEN
C     WRITE(6,*)' qel: inside ENUCL.LT.ENWELL ', ENUCL,ENWELL
         IF ( inipri.LT.10 ) inipri = inipri + 1
C                      Reject (J:R) here all these events
C                      are otherwise rejected in dpmjet
         GOTO 100
C...the interaction is possible, but the nucleon remains inside
C...the nucleus. The nucleus is therefore left excited.
C...We treat this case as a nucleon with 0 kinetic energy.
C       P(5,5) = AMF
C       P(5,4) = AMF
C       P(5,1) = 0.
C       P(5,2) = 0.
C       P(5,3) = 0.
      ELSE IF ( enucl.GE.enwell ) THEN
C     WRITE(6,*)' qel ENUCL.GE.ENWELL ',ENUCL,ENWELL
C...the interaction is possible, the nucleon can exit the nucleus
C...but the nuclear well depth must be subtracted. The nucleus could be
C...left in an excited state.
         pstart = SQRT(P(5,1)**2+P(5,2)**2+P(5,3)**2)
C       P(5,4) = ENUCL-ENWELL + AMF
         pnucl = SQRT(P(5,4)**2-amf**2)
C...The 3-momentum is scaled assuming that the direction remains
C...unaffected
         P(5,1) = P(5,1)*pnucl/pstart
         P(5,2) = P(5,2)*pnucl/pstart
         P(5,3) = P(5,3)*pnucl/pstart
C     WRITE(6,*)' qel new P(5,4) ',P(5,4)
      END IF
CGB-...
      DSIgsu = DSIgsu + dsigev
 
      ga = P(4,4)/P(4,5)
      bgx = P(4,1)/P(4,5)
      bgy = P(4,2)/P(4,5)
      bgz = P(4,3)/P(4,5)
C
      dbetb(1) = bgx/ga
      dbetb(2) = bgy/ga
      dbetb(3) = bgz/ga
 
 
      IF ( NEUdec.EQ.1 .OR. NEUdec.EQ.2 )
     &     CALL PYROBO(6,8,0.0D0,0.0D0,dbetb(1),dbetb(2),dbetb(3))
C
C      PRINT*,' FINE   EVENTO '
      Enu = enu0
      END SUBROUTINE
