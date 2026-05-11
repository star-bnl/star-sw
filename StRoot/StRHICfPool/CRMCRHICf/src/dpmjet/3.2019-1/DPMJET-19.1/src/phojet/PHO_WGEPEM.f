
      SUBROUTINE PHO_WGEPEM(Wghapp,Wghqpm,Imode)
C**********************************************************************
C
C     calculate cross section weights for
C      fully differential equivalent (improved) photon approximation
C     and/or
C      fully differential QPM model with exact one-photon exchange graphs
C
C     (unpolarized lepton beams)
C
C     input:     IMODE     0   flux calculation only
C                          1   flux folded with QPM cross section
C                /POFSRC/  photon and electron momenta
C                /POPRCS/  process type
C                /POCKIN/  kinematics of hard scattering
C
C     output:    WGHAPP  weight of event according to approximation
C                WGHQPM  weight of event according to one-photon exchange
C
C**********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      DOUBLE PRECISION Wghapp , Wghqpm
      INTEGER Imode
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some constants
      INCLUDE 'inc/pocons'
C  gamma-lepton or gamma-hadron vertex information
      INCLUDE 'inc/pofsrc'
C  general process information
      INCLUDE 'inc/poprcs'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      DOUBLE PRECISION aa , alpha1 , alpha2 , bb , cc , ccap , dd , 
     &                 fac , helflx , p1 , p1p2 , p1q2 , p2 , p2q1 , 
     &                 q1kk , q1q2 , q2 , q2kk , qc2 , rho100 , rho1pp , 
     &                 rho200 , rho2pp , rhop08 , rhopm2 , rr , sh , 
     &                 sigqpm , sp , ss , sw0000 , sw0p0m , sw0p0p , 
     &                 sw0pm0 , swp00p , swp0m0 , swp0p0 , swpmpm , 
     &                 swpp00 , swppmm , swpppp , th , tp , uh , w2 , 
     &                 wgheq , wghqq , xcap , xk1 , xk2 , xkam , xkap , 
     &                 xm2 , xq2 , xtm1 , xtm2 , xtm3 , ycap
      DOUBLE PRECISION PHO_ALPHAS , PHO_ALPHAE
 
      INTEGER i , i1 , i2 , idir , ipfl1 , ipfl2 , ipos , k
 
      DIMENSION wgheq(2) , xm2(2) , p1(4) , p2(4) , xk1(4) , xk2(4)
      DIMENSION helflx(6) , sigqpm(6)
 
      Wghapp = 1.D0
      Wghqpm = 0.D0
 
C  strict pt cutoff after putting partons on mass shell,
C  calculated in gamma-gamma CMS
      IF ( (Imode.EQ.1) .AND. (IPAmdl(121).GT.0) ) THEN
         IF ( PTFin.LT.PTWant ) THEN
            IF ( IPAmdl(121).GT.1 ) RETURN
            IF ( (IPAmdl(121).EQ.1) .AND. (MSPr.EQ.14) ) RETURN
         END IF
      END IF
 
C  cross section of sampled event (approximate treatment)
 
C  photon flux
      DO k = 1 , 2
         xm2(k) = AMSrc(k)**2
         IF ( ABS(IGHel(k)).EQ.1 ) THEN
            wgheq(k) = ((1.D0+(1.D0-GYY(k))**2)/GYY(k)-2.D0*xm2(k)
     &                 *GYY(k)/GQ2(k))/(137.D0*2.D0*PI*GQ2(k))
         ELSE
            wgheq(k) = (1.D0-GYY(k))/GYY(k)/(137.D0*PI*GQ2(k))
         END IF
      END DO
 
      w2 = GGEcm*GGEcm
      idir = 0
      wghqq = 1.D0
 
C  direct or single-resolved gam-gam interaction
      IF ( (Imode.GE.1) .AND. (IPRoce.EQ.8) .AND. (MSPr.GE.10) ) THEN
         idir = 1
         wghqq = 0.D0
C  determine final state partons
         DO i = 3 , NHEp
            IF ( ISThep(i).EQ.25 ) GOTO 50
         END DO
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I5)')
     &        'PHO_WGEPEM:ERROR: ' , 
     &        'inconsistent process information (MSPR)' , MSPr
         CALL PHO_ABORT
 50      ipos = i
C  final state flavors
         ipfl1 = ABS(IDHep(ipos+3))
         ipfl2 = ABS(IDHep(ipos+4))
         sh = X1*X2*w2
C  calculate alpha-em
         alpha1 = PHO_ALPHAE(QQAl)
C  calculate alpha-s
         IF ( MSPr.LT.14 ) alpha2 = PHO_ALPHAS(QQAl,3)
C  LO matrix element (8 pi s dsig/dt)
C       QC2 = 4.D0/9.D0 - DBLE(MOD(IPFL2,2))*3.D0/9.D0
         qc2 = Q_Ch2(ipfl2)
         IF ( ipfl2.EQ.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/,5X,A,I12,I3,4I4)')
     &            'PHO_WGEPEM:ERROR: ' , 
     &           'invalid hard process - flavor combination' , 
     &           'EVENT,MSPR,IA,IB,IC,ID:' , KEVent , MSPr , IA , IB , 
     &           IC , ID
         END IF
         IF ( MSPr.EQ.10 ) THEN
            wghqq = -8.D0*PI/(3.D0*sh**2)*alpha1*qc2*alpha2*(U**2+1.D0)
     &              /U*8.D0*PI*sh
         ELSE IF ( MSPr.EQ.11 ) THEN
            wghqq = PI/sh**2*alpha1*qc2*alpha2*(V**2+U**2)/(U*V)
     &              *8.D0*PI*sh
         ELSE IF ( MSPr.EQ.12 ) THEN
            wghqq = -8.D0*PI/(3.D0*sh**2)*alpha1*qc2*alpha2*(V**2+1.D0)
     &              /V*8.D0*PI*sh
         ELSE IF ( MSPr.EQ.13 ) THEN
            wghqq = PI/sh**2*alpha1*qc2*alpha2*(V**2+U**2)/(U*V)
     &              *8.D0*PI*sh
         ELSE IF ( MSPr.EQ.14 ) THEN
            wghqq = 6.D0*PI/sh**2*(alpha1*qc2)**2*(V**2+U**2)/(U*V)
     &              *8.D0*PI*sh
         END IF
      END IF
 
C  fully differential cross section dsig/(dQ_^2 dQ_2^2 dy_1 dy_2 dphi)
      Wghapp = wgheq(1)*wgheq(2)*wghqq/(2.D0*PI)
 
C  full leading-order QPM prediction (Budnev et al.)
 
C  full two-gamma flux
 
      p1q2 = PINi(4,1)*PGAm(4,2) - PINi(1,1)*PGAm(1,2) - PINi(2,1)
     &       *PGAm(2,2) - PINi(3,1)*PGAm(3,2)
      p2q1 = PINi(4,2)*PGAm(4,1) - PINi(1,2)*PGAm(1,1) - PINi(2,2)
     &       *PGAm(2,1) - PINi(3,2)*PGAm(3,1)
      q1q2 = PGAm(4,1)*PGAm(4,2) - PGAm(1,1)*PGAm(1,2) - PGAm(2,1)
     &       *PGAm(2,2) - PGAm(3,1)*PGAm(3,2)
      p1p2 = PINi(4,1)*PINi(4,2) - PINi(1,1)*PINi(1,2) - PINi(2,1)
     &       *PINi(2,2) - PINi(3,1)*PINi(3,2)
      DO i = 1 , 4
         p1(i) = 2.D0*PINi(i,1) - PGAm(i,1)
         p2(i) = 2.D0*PINi(i,2) - PGAm(i,2)
      END DO
      xtm1 = 2.D0*p1q2 - q1q2
      xtm2 = 2.D0*p2q1 - q1q2
      xtm3 = p1(4)*p2(4) - p1(1)*p2(1) - p1(2)*p2(2) - p1(3)*p2(3)
      xcap = q1q2**2 - GQ2(1)*GQ2(2)
      ycap = p1p2**2 - xm2(1)*xm2(2)
      ccap = -xtm3 + q1q2*xtm1*xtm2/xcap
 
      rho1pp = (xtm1**2/xcap+1.D0-4.D0*xm2(1)/GQ2(1))/2.D0
      rho2pp = (xtm2**2/xcap+1.D0-4.D0*xm2(2)/GQ2(2))/2.D0
      rho100 = xtm1**2/xcap - 1.D0
      rho200 = xtm2**2/xcap - 1.D0
      rhopm2 = ccap**2/(GQ2(1)*GQ2(2)) - 2.D0*(rho1pp-1.D0)
     &         *(rho2pp-1.D0)
      rhop08 = 4.D0*xtm1*xtm2*ccap/xcap/SQRT(GQ2(1)*GQ2(2))
      ss = 2.D0*p1p2 + xm2(1) + xm2(2)
 
      helflx(1) = 4.D0*rho1pp*rho2pp
      helflx(2) = rhopm2
      helflx(3) = 2.D0*rho1pp*rho200
      helflx(4) = 2.D0*rho100*rho2pp
      helflx(5) = rho100*rho200
      helflx(6) = -rhop08
 
C  only flux calculation
 
      IF ( idir.EQ.0 ) THEN
         IF ( (IGHel(1).EQ.1) .AND. (IGHel(2).EQ.1) ) THEN
            WEIght = helflx(1)
         ELSE IF ( (IGHel(1).EQ.1) .AND. (IGHel(2).EQ.0) ) THEN
            WEIght = helflx(3)
         ELSE IF ( (IGHel(1).EQ.0) .AND. (IGHel(2).EQ.1) ) THEN
            WEIght = helflx(4)
         ELSE IF ( (IGHel(1).EQ.0) .AND. (IGHel(2).EQ.0) ) THEN
            WEIght = helflx(5)
         ELSE IF ( (IGHel(1).EQ.-1) .AND. (IGHel(2).EQ.-1) ) THEN
            WEIght = helflx(1)
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I3)')
     &            'PHO_GGEPEM:ERROR: invalid photon helicities: ' , 
     &           IGHel
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I12)')
     &            'PHO_GGEPEM: event rejected (KEVENT)' , KEVent
            WEIght = 0.D0
         END IF
 
C  fully differential cross section dsig/(dQ_^2 dQ_2^2 dy_1 dy_2 dphi)
         Wghqpm = WEIght/(137.D0**2*16.D0*PI**4*GQ2(1)*GQ2(2))
     &            *SQRT(xcap/ycap)*PI*ss/(2.D0*ycap)*PINi(4,1)*PINi(4,2)
 
      ELSE
 
C  flux folded with cross section
C  polarized, leading order gam gam --> q qbar cross sections
 
         DO i = 1 , 6
            sigqpm(i) = 0.D0
         END DO
C  momenta of produced parton pair
         i1 = ipos + 3
         i2 = ipos + 4
         DO k = 1 , 4
            xk1(k) = PHEp(k,i1)
            xk2(k) = PHEp(k,i2)
         END DO
         xq2 = PHEp(5,i2)**2
 
         IF ( MSPr.EQ.14 ) THEN
C  direct photon-photon interaction
            xkap = xq2 - (PGAm(4,1)-xk1(4))**2 + (PGAm(1,1)-xk1(1))
     &             **2 + (PGAm(2,1)-xk1(2))**2 + (PGAm(3,1)-xk1(3))**2
            xkam = xq2 - (PGAm(4,1)-xk2(4))**2 + (PGAm(1,1)-xk2(1))
     &             **2 + (PGAm(2,1)-xk2(2))**2 + (PGAm(3,1)-xk2(3))**2
            cc = q1q2
            aa = xkap*xkam - GQ2(1)*GQ2(2)
            bb = cc**2 - xkap*xkam
            dd = cc**2 - GQ2(1)*GQ2(2)
            rr = -xq2 + w2*aa/(4.D0*dd)
            q1kk = q1q2 - GQ2(1)
            q2kk = q1q2 - GQ2(2)
            fac = 192.D0*(PI*alpha1*qc2/(xkap*xkam))
     &            **2/(4.D0*SQRT(xcap))
 
         ELSE
C  single-resolved photon-hadron interactions
C  Mandelstam variables
            IF ( MSPr.LE.11 ) THEN
               th = (PGAm(4,1)-xk1(4))**2 - (PGAm(1,1)-xk1(1))
     &              **2 - (PGAm(2,1)-xk1(2))**2 - (PGAm(3,1)-xk1(3))**2
               uh = (PGAm(4,1)-xk2(4))**2 - (PGAm(1,1)-xk2(1))
     &              **2 - (PGAm(2,1)-xk2(2))**2 - (PGAm(3,1)-xk2(3))**2
            ELSE
               th = (PGAm(4,2)-xk2(4))**2 - (PGAm(1,2)-xk2(1))
     &              **2 - (PGAm(2,2)-xk2(2))**2 - (PGAm(3,2)-xk2(3))**2
               uh = (PGAm(4,2)-xk1(4))**2 - (PGAm(1,2)-xk1(1))
     &              **2 - (PGAm(2,2)-xk1(2))**2 - (PGAm(3,2)-xk1(3))**2
            END IF
            V = th/sh
            U = uh/sh
         END IF
 
         WEIght = 0.D0
         IF ( (IGHel(1).EQ.1) .AND. (IGHel(2).EQ.1) ) THEN
            IF ( (MSPr.EQ.10) .OR. (MSPr.EQ.12) ) THEN
               IF ( MSPr.EQ.10 ) THEN
                  q2 = -GQ2(1)
                  sp = sh - xq2
                  tp = uh - xq2
               ELSE
                  q2 = -GQ2(2)
                  sp = sh - xq2
                  tp = th - xq2
               END IF
               sigqpm(1) = -32.D0*PI**2*4.D0/3.D0*alpha1*qc2*alpha2*
     &                     (sp*tp*(2.D0*q2**4-4.D0*q2*sp**3-
     &                     2.D0*q2**3*(3*sp+tp)+sp**2*(sp**2+tp**2)
     &                     +q2**2*(7.D0*sp**2+2.D0*sp*tp+tp**2))
     &                     -2.D0*(2.D0*sp**3*tp*(sp+tp)
     &                     +q2**3*(sp**2+6.D0*sp*tp+tp**2)
     &                     -2.D0*q2**2*sp*(sp**2+4.D0*sp*tp+3.D0*tp**2)
     &                     +q2*sp*(sp**3+sp**2*tp-sp*tp**2+tp**3))
     &                     *xq2+4.D0*(2.D0*q2**2-sp**2)*(sp+tp)
     &                     **2*xq2**2)
     &                     /(sp**2*tp**2*((q2-sp)**2-4.D0*q2*xq2))
               WEIght = helflx(1)*sigqpm(1)/(2.D0*(sh+GQ2(1)+GQ2(2)))
            ELSE IF ( (MSPr.EQ.11) .OR. (MSPr.EQ.13) ) THEN
               IF ( MSPr.EQ.11 ) THEN
                  q2 = -GQ2(1)
               ELSE
                  q2 = -GQ2(2)
               END IF
               sp = sh
               tp = uh
               sigqpm(1) = -32.D0*PI**2/2.D0*alpha1*qc2*alpha2*
     &                     (-((q2**2+sp**2)
     &                     *tp*(q2**3-sp**3-3.D0*sp**2*tp-4.D0*sp*tp**2-
     &                     2.D0*tp**3-3.D0*q2**2*(sp+tp)
     &                     +q2*(3.D0*sp**2+6.D0*sp*tp+4.D0*tp**2)))
     &                     +(3.D0*q2**5-q2**4*(11.D0*sp+10.D0*tp)
     &                     +4.D0*q2**3*(4.D0*sp**2+5.D0*sp*tp+
     &                     4.D0*tp**2)
     &                     +q2*sp**2*(5.D0*sp**2+4.D0*sp*tp+8.D0*tp**2)
     &                     -4.D0*q2**2*(3.D0*sp**3+3.D0*sp**2*tp+
     &                     4.D0*sp*tp**2+2.D0*tp**3)
     &                     -sp**2*(sp**3+2.D0*sp**2*tp+8.D0*sp*tp**2+
     &                     8.D0*tp**3))
     &                     *xq2+(11.D0*q2**4-10.D0*q2**3*(3.D0*sp+
     &                     2.D0*tp)-2.D0*q2*sp**2*(7.D0*sp+2.D0*tp)
     &                     +2.D0*q2**2*(15.D0*sp**2+10.D0*sp*tp+
     &                     6.D0*tp**2)
     &                     +sp**2*(3.D0*sp**2+4.D0*sp*tp+12.D0*tp**2))
     &                     *xq2**2+8.D0*(q2**3-sp**2*tp-q2**2*(sp+tp))
     &                     *xq2**3+2.D0*(q2**2+sp**2)*xq2**4)
     &                     /((q2-sp)**2*(-tp+xq2)**2*(q2-sp-tp+xq2)**2)
               WEIght = helflx(1)*sigqpm(1)/(2.D0*(sh+GQ2(1)+GQ2(2)))
            ELSE IF ( MSPr.EQ.14 ) THEN
               swpmpm = 4.D0*cc**2*rr*(w2-2.D0*rr)
               swpppp = swpmpm + 2.D0*(cc**2+bb)*(aa-4.D0*rr*cc)
               swppmm = 8.D0*rr*cc*(xkap*xkam-rr*cc) - 2.D0*xkap*xkam*aa
               sigqpm(1) = (swpppp+swpmpm)/2.D0*fac
               sigqpm(2) = swppmm*fac
               WEIght = helflx(1)*sigqpm(1) + helflx(2)*sigqpm(2)
            END IF
         ELSE IF ( (IGHel(1).EQ.1) .AND. (IGHel(2).EQ.0) ) THEN
            IF ( MSPr.EQ.12 ) THEN
               q2 = -GQ2(2)
               sp = sh - xq2
               tp = th - xq2
               sigqpm(3) = 32.D0*PI**2*8.D0/3.D0*alpha1*qc2*alpha2*q2*
     &                     (-(sp**2*tp**2*(-q2+sp+tp))
     &                     +sp*tp*(2.D0*q2**2+3.D0*sp**2+2.D0*sp*tp-
     &                     tp**2-2.D0*q2*(3*sp+tp))
     &                     *xq2-2.D0*(q2*(sp**2+6.D0*sp*tp+tp**2)
     &                     -2.D0*sp*(sp**2+4.D0*sp*tp+3.D0*tp**2))
     &                     *xq2**2+8.D0*(sp+tp)**2*xq2**3)
     &                     /(sp**2*tp**2*((q2-sp)**2-4.D0*q2*xq2))
               WEIght = helflx(3)*sigqpm(3)/(2.D0*(sh+GQ2(2)))
            ELSE IF ( MSPr.EQ.13 ) THEN
               q2 = -GQ2(2)
               sp = sh
               tp = th
               sigqpm(3) = 32.D0*PI**2*2.D0*alpha1*qc2*alpha2*
     &                     (-q2*(sp*tp*(-q2+sp+tp)+(q2**2-q2*sp-2*sp*tp)
     &                     *xq2+sp*xq2**2))
     &                     /((q2-sp)**2*(-tp+xq2)*(q2-sp-tp+xq2))
               WEIght = helflx(3)*sigqpm(3)/(2.D0*(sh+GQ2(2)))
            ELSE IF ( MSPr.EQ.14 ) THEN
               swp0m0 = 4.D0*rr*GQ2(2)
     &                  *(-cc**2*GQ2(1)*w2-xkap*xkam*q1kk**2)/dd
               swp0p0 = -swp0m0 + 2.D0*GQ2(2)*GQ2(1)**2*w2*bb/dd
               swpp00 = 2.D0*w2*bb*(aa-2.D0*cc*rr)*SQRT(GQ2(1)*GQ2(2))
     &                  /dd
               swp00p = 4.D0*rr*(cc**2*(GQ2(1)*q2kk+GQ2(2)*q1kk)
     &                  +xkap*xkam*q1kk*q2kk)*SQRT(GQ2(1)*GQ2(2))/dd
               sw0pm0 = -swp00p - 2.D0*GQ2(1)*GQ2(2)
     &                  *w2*bb*SQRT(GQ2(1)*GQ2(2))/dd
               sigqpm(3) = swp0p0*fac
               sigqpm(6) = (swpp00+sw0pm0)/2.D0*fac
               WEIght = helflx(3)*sigqpm(3) + helflx(6)*sigqpm(6)/2.D0
            END IF
         ELSE IF ( (IGHel(1).EQ.0) .AND. (IGHel(2).EQ.1) ) THEN
            IF ( MSPr.EQ.10 ) THEN
               q2 = -GQ2(1)
               sp = sh - xq2
               tp = uh - xq2
               sigqpm(4) = 32.D0*PI**2*8.D0/3.D0*alpha1*qc2*alpha2*q2*
     &                     (-(sp**2*tp**2*(-q2+sp+tp))
     &                     +sp*tp*(2.D0*q2**2+3.D0*sp**2+2.D0*sp*tp-
     &                     tp**2-2.D0*q2*(3*sp+tp))
     &                     *xq2-2.D0*(q2*(sp**2+6.D0*sp*tp+tp**2)
     &                     -2.D0*sp*(sp**2+4.D0*sp*tp+3.D0*tp**2))
     &                     *xq2**2+8.D0*(sp+tp)**2*xq2**3)
     &                     /(sp**2*tp**2*((q2-sp)**2-4.D0*q2*xq2))
               WEIght = helflx(4)*sigqpm(4)/(2.D0*(sh+GQ2(1)))
            ELSE IF ( MSPr.EQ.11 ) THEN
               q2 = -GQ2(1)
               sp = sh
               tp = th
               sigqpm(4) = 32.D0*PI**2*2.D0*alpha1*qc2*alpha2*
     &                     (-q2*(sp*tp*(-q2+sp+tp)+(q2**2-q2*sp-2*sp*tp)
     &                     *xq2+sp*xq2**2))
     &                     /((q2-sp)**2*(-tp+xq2)*(q2-sp-tp+xq2))
               WEIght = helflx(4)*sigqpm(4)/(2.D0*(sh+GQ2(2)))
            ELSE IF ( MSPr.EQ.14 ) THEN
               sw0p0m = 4.D0*rr*GQ2(1)
     &                  *(-cc**2*GQ2(2)*w2-xkap*xkam*q2kk**2)/dd
               sw0p0p = -sw0p0m + 2.D0*GQ2(1)*GQ2(2)**2*w2*bb/dd
               swpp00 = 2.D0*w2*bb*(aa-2.D0*cc*rr)*SQRT(GQ2(1)*GQ2(2))
     &                  /dd
               swp00p = 4.D0*rr*(cc**2*(GQ2(1)*q2kk+GQ2(2)*q1kk)
     &                  +xkap*xkam*q1kk*q2kk)*SQRT(GQ2(1)*GQ2(2))/dd
               sw0pm0 = -swp00p - 2.D0*GQ2(1)*GQ2(2)
     &                  *w2*bb*SQRT(GQ2(1)*GQ2(2))/dd
               sigqpm(4) = sw0p0p*fac
               sigqpm(6) = (swpp00+sw0pm0)/2.D0*fac
               WEIght = helflx(4)*sigqpm(4) + helflx(6)*sigqpm(6)/2.D0
            END IF
         ELSE IF ( (IGHel(1).EQ.0) .AND. (IGHel(2).EQ.0) ) THEN
            IF ( MSPr.EQ.14 ) THEN
               sw0000 = 2.D0*GQ2(1)*GQ2(2)*w2*w2*aa*bb/dd**2
               sigqpm(5) = sw0000*fac
               WEIght = helflx(5)*sigqpm(5)
            END IF
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I3)')
     &            'PHO_GGEPEM:ERROR: invalid photon helicities: ' , 
     &           IGHel
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I12)')
     &            'PHO_GGEPEM: event rejected (KEVENT)' , KEVent
            WEIght = 0.D0
         END IF
 
C  fully differential cross section dsig/(dQ_^2 dQ_2^2 dy_1 dy_2 dphi)
 
         Wghqpm = WEIght/(137.D0**2*16.D0*PI**4*GQ2(1)*GQ2(2))
     &            *SQRT(xcap/ycap)*PI*ss/(2.D0*ycap)*PINi(4,1)*PINi(4,2)
 
      END IF
 
      END SUBROUTINE
