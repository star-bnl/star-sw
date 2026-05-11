
C***********************************************************************
C...SaSgam version 2 - parton distributions of the photon
C...by Gerhard A. Schuler and Torbjorn Sjostrand
C...For further information see Z. Phys. C68 (1995) 607
C...and Phys. Lett. B376 (1996) 193.
 
C...18 January 1996: original code.
C...22 July 1996: calculation of BETA moved in SASBEH.
 
C!!!Note that one further call parameter - IP2 - has been added
C!!!to the SASGAM argument list compared with version 1.
 
C...The user should only need to call the SASGAM routine,
C...which in turn calls the auxiliary routines SASVMD, SASANO,
C...SASBEH and SASDIR. The package is self-contained.
 
C...One particular aspect of these parametrizations is that F2 for
C...the photon is not obtained just as the charge-squared-weighted
C...sum of quark distributions, but differ in the treatment of
C...heavy flavours (in F2 the DIS relation W2 = Q2*(1-x)/x restricts
C...the kinematics range of heavy-flavour production, but the same
C...kinematics is not relevant e.g. for jet production) and, for the
C...'MSbar' fits, in the addition of a Cgamma term related to the
C...separation of direct processes. Schematically:
C...PDF = VMD (rho, omega, phi) + anomalous (d, u, s, c, b).
C...F2  = VMD (rho, omega, phi) + anomalous (d, u, s) +
C...      Bethe-Heitler (c, b) (+ Cgamma (d, u, s)).
C...The J/psi and Upsilon states have not been included in the VMD sum,
C...but low c and b masses in the other components should compensate
C...for this in a duality sense.
 
C...The calling sequence is the following:
C     CALL SASGAM(ISET,X,Q2,P2,IP2,F2GM,XPDFGM)
C...with the following declaration statement:
C     DIMENSION XPDFGM(-6:6)
C...and, optionally, further information in:
C     COMMON/SASCOM/XPVMD(-6:6),XPANL(-6:6),XPANH(-6:6),XPBEH(-6:6),
C    &XPDIR(-6:6)
C     COMMON/SASVAL/VXPVMD(-6:6),VXPANL(-6:6),VXPANH(-6:6),VXPDGM(-6:6)
C...Input:  ISET = 1 : SaS set 1D ('DIS',   Q0 = 0.6 GeV)
C                = 2 : SaS set 1M ('MSbar', Q0 = 0.6 GeV)
C                = 3 : SaS set 2D ('DIS',   Q0 =  2  GeV)
C                = 4 : SaS set 2M ('MSbar', Q0 =  2  GeV)
C           X : x value.
C           Q2 : Q2 value.
C           P2 : P2 value; should be = 0. for an on-shell photon.
C           IP2 : scheme used to evaluate off-shell anomalous component.
C               = 0 : recommended default, see = 7.
C               = 1 : dipole dampening by integration; very time-consuming.
C               = 2 : P_0^2 = max( Q_0^2, P^2 )
C               = 3 : P_0^2 = Q_0^2 + P^2.
C               = 4 : P_{eff} that preserves momentum sum.
C               = 5 : P_{int} that preserves momentum and average
C                     evolution range.
C               = 6 : P_{eff}, matched to P_0 in P2 -> Q2 limit.
C               = 7 : P_{eff}, matched to P_0 in P2 -> Q2 limit.
C...Output: F2GM : F2 value of the photon (including factors of alpha_em).
C           XPFDGM :  x times parton distribution functions of the photon,
C               with elements 0 = g, 1 = d, 2 = u, 3 = s, 4 = c, 5 = b,
C               6 = t (always empty!), - for antiquarks (result is same).
C...The breakdown by component is stored in the commonblock SASCOM,
C               with elements as above.
C           XPVMD : rho, omega, phi VMD part only of output.
C           XPANL : d, u, s anomalous part only of output.
C           XPANH : c, b anomalous part only of output.
C           XPBEH : c, b Bethe-Heitler part only of output.
C           XPDIR : Cgamma (direct contribution) part only of output.
C...The above arrays do not distinguish valence and sea contributions,
C...although this information is available internally. The additional
C...commonblock SASVAL provides the valence part only of the above
C...distributions. Array names VXPVMD, VXPANL and VXPANH correspond
C...to XPVMD, XPANL and XPANH, while XPBEH and XPDIR are valence only
C...and therefore not given doubly. VXPDGM gives the sum of valence
C...parts, and so matches XPDFGM. The difference, i.e. XPVMD-VXPVMD
C...and so on, gives the sea part only.
C***********************************************************************
 
      SUBROUTINE PHO_SASGAM(Iset,X,Q2,P2,Ip2,F2gm,Xpdfgm)
      IMPLICIT NONE
      REAL aem , aem2pi , alam , chsq , F2gm , facnor , facq , facs , 
     &     facud , fomega , fphi , fracu , frho , P2 , p2mx , p2mxa , 
     &     p2mxb , pmb , pmc , pmphi
      REAL pmrho , q0 , q02 , Q2 , q2a , q2step , vxpga , X , xfval , 
     &     xpbh , Xpdfgm , xpf2 , xpga
      INTEGER Ip2 , Iset , istep , kf , kfl , nstep
C...Purpose: to construct the F2 and parton distributions of the photon
C...by summing homogeneous (VMD) and inhomogeneous (anomalous) terms.
C...For F2, c and b are included by the Bethe-Heitler formula;
C...in the 'MSbar' scheme additionally a Cgamma term is added.
      SAVE 
      DIMENSION Xpdfgm(-6:6)
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      INCLUDE 'inc/sascom'
      INCLUDE 'inc/sasval'
      SAVE /SASCOM/  , /SASVAL/ 
 
C...Temporary array.
      DIMENSION xpga(-6:6) , vxpga(-6:6)
C...Charm and bottom masses (low to compensate for J/psi etc.).
      DATA pmc/1.3/ , pmb/4.6/
C...alpha_em and alpha_em/(2*pi).
      DATA aem/0.007297/ , aem2pi/0.0011614/
C...Lambda value for 4 flavours.
      DATA alam/0.20/
C...Mixture u/(u+d), = 0.5 for incoherent and = 0.8 for coherent sum.
      DATA fracu/0.8/
C...VMD couplings f_V**2/(4*pi).
      DATA frho/2.20/ , fomega/23.6/ , fphi/18.4/
C...Masses for rho (=omega) and phi.
      DATA pmrho/0.770/ , pmphi/1.020/
C...Number of points in integration for IP2=1.
      DATA nstep/100/
 
C...Reset output.
      F2gm = 0.
      DO kfl = -6 , 6
         Xpdfgm(kfl) = 0.
         XPVmd(kfl) = 0.
         XPAnl(kfl) = 0.
         XPAnh(kfl) = 0.
         XPBeh(kfl) = 0.
         XPDir(kfl) = 0.
         VXPvmd(kfl) = 0.
         VXPanl(kfl) = 0.
         VXPanh(kfl) = 0.
         VXPdgm(kfl) = 0.
      END DO
 
C...Check that input sensible.
      IF ( Iset.LE.0 .OR. Iset.GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,*)
     &         ' FATAL ERROR: SaSgam called for unknown set'
         IF ( LPRi.GT.4 ) WRITE (LO,*) ' ISET = ' , Iset
         STOP
      END IF
      IF ( X.LE.0. .OR. X.GT.1. ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,*)
     &         ' FATAL ERROR: SaSgam called for unphysical x'
         IF ( LPRi.GT.4 ) WRITE (LO,*) ' X = ' , X
         STOP
      END IF
 
C...Set Q0 cut-off parameter as function of set used.
      IF ( Iset.LE.2 ) THEN
         q0 = 0.6
      ELSE
         q0 = 2.
      END IF
      q02 = q0**2
 
C...Scale choice for off-shell photon; common factors.
      q2a = Q2
      facnor = 1.
      IF ( Ip2.EQ.1 ) THEN
         p2mx = P2 + q02
         q2a = Q2 + P2*q02/MAX(q02,Q2)
         facnor = LOG(Q2/q02)/nstep
      ELSE IF ( Ip2.EQ.2 ) THEN
         p2mx = MAX(P2,q02)
      ELSE IF ( Ip2.EQ.3 ) THEN
         p2mx = P2 + q02
         q2a = Q2 + P2*q02/MAX(q02,Q2)
      ELSE IF ( Ip2.EQ.4 ) THEN
         p2mx = Q2*(q02+P2)/(Q2+P2)*EXP(P2*(Q2-q02)/((Q2+P2)*(q02+P2)))
      ELSE IF ( Ip2.EQ.5 ) THEN
         p2mxa = Q2*(q02+P2)/(Q2+P2)*EXP(P2*(Q2-q02)/((Q2+P2)*(q02+P2)))
         p2mx = q0*SQRT(p2mxa)
         facnor = LOG(Q2/p2mxa)/LOG(Q2/p2mx)
      ELSE IF ( Ip2.EQ.6 ) THEN
         p2mx = Q2*(q02+P2)/(Q2+P2)*EXP(P2*(Q2-q02)/((Q2+P2)*(q02+P2)))
         p2mx = MAX(0.,1.-P2/Q2)*p2mx + MIN(1.,P2/Q2)*MAX(P2,q02)
      ELSE
         p2mxa = Q2*(q02+P2)/(Q2+P2)*EXP(P2*(Q2-q02)/((Q2+P2)*(q02+P2)))
         p2mx = q0*SQRT(p2mxa)
         p2mxb = p2mx
         p2mx = MAX(0.,1.-P2/Q2)*p2mx + MIN(1.,P2/Q2)*MAX(P2,q02)
         p2mxb = MAX(0.,1.-P2/Q2)*p2mxb + MIN(1.,P2/Q2)*p2mxa
         facnor = LOG(Q2/p2mxa)/LOG(Q2/p2mxb)
      END IF
 
C...Call VMD parametrization for d quark and use to give rho, omega,
C...phi. Note dipole dampening for off-shell photon.
      CALL PHO_SASVMD(Iset,1,X,q2a,p2mx,alam,xpga,vxpga)
      xfval = vxpga(1)
      xpga(1) = xpga(2)
      xpga(-1) = xpga(-2)
      facud = aem*(1./frho+1./fomega)*(pmrho**2/(pmrho**2+P2))**2
      facs = aem*(1./fphi)*(pmphi**2/(pmphi**2+P2))**2
      DO kfl = -5 , 5
         XPVmd(kfl) = (facud+facs)*xpga(kfl)
      END DO
      XPVmd(1) = XPVmd(1) + (1.-fracu)*facud*xfval
      XPVmd(2) = XPVmd(2) + fracu*facud*xfval
      XPVmd(3) = XPVmd(3) + facs*xfval
      XPVmd(-1) = XPVmd(-1) + (1.-fracu)*facud*xfval
      XPVmd(-2) = XPVmd(-2) + fracu*facud*xfval
      XPVmd(-3) = XPVmd(-3) + facs*xfval
      VXPvmd(1) = (1.-fracu)*facud*xfval
      VXPvmd(2) = fracu*facud*xfval
      VXPvmd(3) = facs*xfval
      VXPvmd(-1) = (1.-fracu)*facud*xfval
      VXPvmd(-2) = fracu*facud*xfval
      VXPvmd(-3) = facs*xfval
 
      IF ( Ip2.NE.1 ) THEN
C...Anomalous parametrizations for different strategies
C...for off-shell photons; except full integration.
 
C...Call anomalous parametrization for d + u + s.
         CALL PHO_SASANO(-3,X,q2a,p2mx,alam,xpga,vxpga)
         DO kfl = -5 , 5
            XPAnl(kfl) = facnor*xpga(kfl)
            VXPanl(kfl) = facnor*vxpga(kfl)
         END DO
 
C...Call anomalous parametrization for c and b.
         CALL PHO_SASANO(4,X,q2a,p2mx,alam,xpga,vxpga)
         DO kfl = -5 , 5
            XPAnh(kfl) = facnor*xpga(kfl)
            VXPanh(kfl) = facnor*vxpga(kfl)
         END DO
         CALL PHO_SASANO(5,X,q2a,p2mx,alam,xpga,vxpga)
         DO kfl = -5 , 5
            XPAnh(kfl) = XPAnh(kfl) + facnor*xpga(kfl)
            VXPanh(kfl) = VXPanh(kfl) + facnor*vxpga(kfl)
         END DO
 
      ELSE
C...Special option: loop over flavours and integrate over k2.
         DO kf = 1 , 5
            DO istep = 1 , nstep
               q2step = q02*(Q2/q02)**((istep-0.5)/nstep)
               IF ( .NOT.((kf.EQ.4 .AND. q2step.LT.pmc**2) .OR. 
     &              (kf.EQ.5 .AND. q2step.LT.pmb**2)) ) THEN
                  CALL PHO_SASVMD(0,kf,X,Q2,q2step,alam,xpga,vxpga)
                  facq = aem2pi*(q2step/(q2step+P2))**2*facnor
                  IF ( MOD(kf,2).EQ.0 ) facq = facq*(8./9.)
                  IF ( MOD(kf,2).EQ.1 ) facq = facq*(2./9.)
                  DO kfl = -5 , 5
                     IF ( kf.LE.3 ) XPAnl(kfl) = XPAnl(kfl)
     &                    + facq*xpga(kfl)
                     IF ( kf.GE.4 ) XPAnh(kfl) = XPAnh(kfl)
     &                    + facq*xpga(kfl)
                     IF ( kf.LE.3 ) VXPanl(kfl) = VXPanl(kfl)
     &                    + facq*vxpga(kfl)
                     IF ( kf.GE.4 ) VXPanh(kfl) = VXPanh(kfl)
     &                    + facq*vxpga(kfl)
                  END DO
               END IF
            END DO
         END DO
      END IF
 
C...Call Bethe-Heitler term expression for charm and bottom.
      CALL PHO_SASBEH(4,X,Q2,P2,pmc**2,xpbh)
      XPBeh(4) = xpbh
      XPBeh(-4) = xpbh
      CALL PHO_SASBEH(5,X,Q2,P2,pmb**2,xpbh)
      XPBeh(5) = xpbh
      XPBeh(-5) = xpbh
 
C...For MSbar subtraction call C^gamma term expression for d, u, s.
      IF ( Iset.EQ.2 .OR. Iset.EQ.4 ) THEN
         CALL PHO_SASDIR(X,Q2,P2,q02,xpga)
         DO kfl = -5 , 5
            XPDir(kfl) = xpga(kfl)
         END DO
      END IF
 
C...Store result in output array.
      DO kfl = -5 , 5
         chsq = 1./9.
         IF ( ABS(kfl).EQ.2 .OR. ABS(kfl).EQ.4 ) chsq = 4./9.
         xpf2 = XPVmd(kfl) + XPAnl(kfl) + XPBeh(kfl) + XPDir(kfl)
         IF ( kfl.NE.0 ) F2gm = F2gm + chsq*xpf2
         Xpdfgm(kfl) = XPVmd(kfl) + XPAnl(kfl) + XPAnh(kfl)
         VXPdgm(kfl) = VXPvmd(kfl) + VXPanl(kfl) + VXPanh(kfl)
      END DO
 
      END SUBROUTINE
