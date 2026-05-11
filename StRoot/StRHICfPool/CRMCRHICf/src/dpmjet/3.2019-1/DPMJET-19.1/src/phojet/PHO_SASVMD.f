
      SUBROUTINE PHO_SASVMD(Iset,Kf,X,Q2,P2,Alam,Xpga,Vxpga)
      IMPLICIT NONE
      REAL aem , aem2pi , Alam , alam3 , alam5 , P2 , p2div , p2eff , 
     &     pmb , pmc , Q2 , q2div , q2eff , s , s2 , s3 , s4 , sbt , 
     &     sch , sll
      REAL Vxpga , X , x1 , xbot , xchm , xglu , xl , Xpga , xsea , 
     &     xsea0 , xval
      INTEGER Iset , Kf , kfa , kfl , nfp , nfq
C...Purpose: to evaluate the VMD parton distributions of a photon,
C...evolved homogeneously from an initial scale P2 to Q2.
C...Does not include dipole suppression factor.
C...ISET is parton distribution set, see above;
C...additionally ISET=0 is used for the evolution of an anomalous photon
C...which branched at a scale P2 and then evolved homogeneously to Q2.
C...ALAM is the 4-flavour Lambda, which is automatically converted
C...to 3- and 5-flavour equivalents as needed.
      SAVE 
      DIMENSION Xpga(-6:6) , Vxpga(-6:6)
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      DATA pmc/1.3/ , pmb/4.6/ , aem/0.007297/ , aem2pi/0.0011614/
 
C...Reset output.
      DO kfl = -6 , 6
         Xpga(kfl) = 0.
         Vxpga(kfl) = 0.
      END DO
      kfa = ABS(Kf)
 
C...Calculate Lambda; protect against unphysical Q2 and P2 input.
      alam3 = Alam*(pmc/Alam)**(2./27.)
      alam5 = Alam*(Alam/pmb)**(2./23.)
      p2eff = MAX(P2,1.2*alam3**2)
      IF ( kfa.EQ.4 ) p2eff = MAX(p2eff,pmc**2)
      IF ( kfa.EQ.5 ) p2eff = MAX(p2eff,pmb**2)
      q2eff = MAX(Q2,p2eff)
 
C...Find number of flavours at lower and upper scale.
      nfp = 4
      IF ( p2eff.LT.pmc**2 ) nfp = 3
      IF ( p2eff.GT.pmb**2 ) nfp = 5
      nfq = 4
      IF ( q2eff.LT.pmc**2 ) nfq = 3
      IF ( q2eff.GT.pmb**2 ) nfq = 5
 
C...Find s as sum of 3-, 4- and 5-flavour parts.
      s = 0.
      IF ( nfp.EQ.3 ) THEN
         q2div = pmc**2
         IF ( nfq.EQ.3 ) q2div = q2eff
         s = s + (6./27.)*LOG(LOG(q2div/alam3**2)/LOG(p2eff/alam3**2))
      END IF
      IF ( nfp.LE.4 .AND. nfq.GE.4 ) THEN
         p2div = p2eff
         IF ( nfp.EQ.3 ) p2div = pmc**2
         q2div = q2eff
         IF ( nfq.EQ.5 ) q2div = pmb**2
         s = s + (6./25.)*LOG(LOG(q2div/Alam**2)/LOG(p2div/Alam**2))
      END IF
      IF ( nfq.EQ.5 ) THEN
         p2div = pmb**2
         IF ( nfp.EQ.5 ) p2div = p2eff
         s = s + (6./23.)*LOG(LOG(q2eff/alam5**2)/LOG(p2div/alam5**2))
      END IF
 
C...Calculate frequent combinations of x and s.
      x1 = 1. - X
      xl = -LOG(X)
      s2 = s**2
      s3 = s**3
      s4 = s**4
 
C...Evaluate homogeneous anomalous parton distributions below or
C...above threshold.
      IF ( Iset.EQ.0 ) THEN
         IF ( Q2.LE.P2 .OR. (kfa.EQ.4 .AND. Q2.LT.pmc**2) .OR. 
     &        (kfa.EQ.5 .AND. Q2.LT.pmb**2) ) THEN
            xval = X*1.5*(X**2+x1**2)
            xglu = 0.
            xsea = 0.
         ELSE
            xval = (1.5/(1.-0.197*s+4.33*s2)*X**2+(1.5+2.10*s)
     &             /(1.+3.29*s)*x1**2+5.23*s/(1.+1.17*s+19.9*s3)*X*x1)
     &             *X**(1./(1.+1.5*s))*(1.-X**2)**(2.667*s)
            xglu = 4.*s/(1.+4.76*s+15.2*s2+29.3*s4)
     &             *X**(-2.03*s/(1.+2.44*s))*(x1*xl)**(1.333*s)
     &             *((4.*X**2+7.*X+4.)*x1/3.-2.*X*(1.+X)*xl)
            xsea = s2/(1.+4.54*s+8.19*s2+8.05*s3)
     &             *X**(-1.54*s/(1.+1.29*s))*x1**(2.667*s)
     &             *((8.-73.*X+62.*X**2)*x1/9.+(3.-8.*X**2/3.)
     &             *X*xl+(2.*X-1.)*X*xl**2)
         END IF
 
C...Evaluate set 1D parton distributions below or above threshold.
      ELSE IF ( Iset.EQ.1 ) THEN
         IF ( Q2.LE.P2 .OR. (kfa.EQ.4 .AND. Q2.LT.pmc**2) .OR. 
     &        (kfa.EQ.5 .AND. Q2.LT.pmb**2) ) THEN
            xval = 1.294*X**0.80*x1**0.76
            xglu = 1.273*X**0.40*x1**1.76
            xsea = 0.100*x1**3.76
         ELSE
            xval = 1.294/(1.+0.252*s+3.079*s2)*X**(0.80-0.13*s)
     &             *x1**(0.76+0.667*s)*xl**(2.*s)
            xglu = 7.90*s/(1.+5.50*s)*EXP(-5.16*s)
     &             *X**(-1.90*s/(1.+3.60*s))*x1**1.30*xl**(0.50+3.*s)
     &             + 1.273*EXP(-10.*s)*X**0.40*x1**(1.76+3.*s)
            xsea = (0.1-0.397*s2+1.121*s3)/(1.+5.61*s2+5.26*s3)
     &             *X**(-7.32*s2/(1.+10.3*s2))
     &             *x1**((3.76+15.*s+12.*s2)/(1.+4.*s))
            xsea0 = 0.100*x1**3.76
         END IF
 
C...Evaluate set 1M parton distributions below or above threshold.
      ELSE IF ( Iset.EQ.2 ) THEN
         IF ( Q2.LE.P2 .OR. (kfa.EQ.4 .AND. Q2.LT.pmc**2) .OR. 
     &        (kfa.EQ.5 .AND. Q2.LT.pmb**2) ) THEN
            xval = 0.8477*X**0.51*x1**1.37
            xglu = 3.42*X**0.255*x1**2.37
            xsea = 0.
         ELSE
            xval = 0.8477/(1.+1.37*s+2.18*s2+3.73*s3)*X**(0.51+0.21*s)
     &             *x1**1.37*xl**(2.667*s)
            xglu = 24.*s/(1.+9.6*s+0.92*s2+14.34*s3)*EXP(-5.94*s)
     &             *X**((-0.013-1.80*s)/(1.+3.14*s))*x1**(2.37+0.4*s)
     &             *xl**(0.32+3.6*s) + 3.42*EXP(-12.*s)
     &             *X**0.255*x1**(2.37+3.*s)
            xsea = 0.842*s/(1.+21.3*s-33.2*s2+229.*s3)
     &             *X**((0.13-2.90*s)/(1.+5.44*s))*x1**(3.45+0.5*s)
     &             *xl**(2.8*s)
            xsea0 = 0.
         END IF
 
C...Evaluate set 2D parton distributions below or above threshold.
      ELSE IF ( Iset.EQ.3 ) THEN
         IF ( Q2.LE.P2 .OR. (kfa.EQ.4 .AND. Q2.LT.pmc**2) .OR. 
     &        (kfa.EQ.5 .AND. Q2.LT.pmb**2) ) THEN
            xval = X**0.46*x1**0.64 + 0.76*X
            xglu = 1.925*x1**2
            xsea = 0.242*x1**4
         ELSE
            xval = (1.+0.186*s)/(1.-0.209*s+1.495*s2)*X**(0.46+0.25*s)
     &             *x1**((0.64+0.14*s+5.*s2)/(1.+s))*xl**(1.9*s)
     &             + (0.76+0.4*s)*X*x1**(2.667*s)
            xglu = (1.925+5.55*s+147.*s2)/(1.-3.59*s+3.32*s2)
     &             *EXP(-18.67*s)
     &             *X**((-5.81*s-5.34*s2)/(1.+29.*s-4.26*s2))
     &             *x1**((2.-5.9*s)/(1.+1.7*s))*xl**(9.3*s/(1.+1.7*s))
            xsea = (0.242-0.252*s+1.19*s2)/(1.-0.607*s+21.95*s2)
     &             *X**(-12.1*s2/(1.+2.62*s+16.7*s2))*x1**4*xl**s
            xsea0 = 0.242*x1**4
         END IF
 
C...Evaluate set 2M parton distributions below or above threshold.
      ELSE IF ( Iset.EQ.4 ) THEN
         IF ( Q2.LE.P2 .OR. (kfa.EQ.4 .AND. Q2.LT.pmc**2) .OR. 
     &        (kfa.EQ.5 .AND. Q2.LT.pmb**2) ) THEN
            xval = 1.168*X**0.50*x1**2.60 + 0.965*X
            xglu = 1.808*x1**2
            xsea = 0.209*x1**4
         ELSE
            xval = (1.168+1.771*s+29.35*s2)*EXP(-5.776*s)
     &             *X**((0.5+0.208*s)/(1.-0.794*s+1.516*s2))
     &             *x1**((2.6+7.6*s)/(1.+5.*s))*xl**(5.15*s/(1.+2.*s))
     &             + (0.965+22.35*s)/(1.+18.4*s)*X*x1**(2.667*s)
            xglu = (1.808+29.9*s)/(1.+26.4*s)*EXP(-5.28*s)
     &             *X**((-5.35*s-10.11*s2)/(1.+31.71*s))
     &             *x1**((2.-7.3*s+4.*s2)/(1.+2.5*s))
     &             *xl**(10.9*s/(1.+2.5*s))
            xsea = (0.209+0.644*s2)/(1.+0.319*s+17.6*s2)
     &             *X**((-0.373*s-7.71*s2)/(1.+0.815*s+11.0*s2))
     &             *x1**(4.+s)*xl**(0.45*s)
            xsea0 = 0.209*x1**4
         END IF
      END IF
 
C...Threshold factors for c and b sea.
      sll = LOG(LOG(q2eff/Alam**2)/LOG(p2eff/Alam**2))
      xchm = 0.
      IF ( Q2.GT.pmc**2 .AND. Q2.GT.1.001*p2eff ) THEN
         sch = MAX(0.,LOG(LOG(pmc**2/Alam**2)/LOG(p2eff/Alam**2)))
         IF ( Iset.EQ.0 ) THEN
            xchm = xsea*(1.-(sch/sll)**2)
         ELSE
            xchm = MAX(0.,xsea-xsea0*x1**(2.667*s))*(1.-sch/sll)
         END IF
      END IF
      xbot = 0.
      IF ( Q2.GT.pmb**2 .AND. Q2.GT.1.001*p2eff ) THEN
         sbt = MAX(0.,LOG(LOG(pmb**2/Alam**2)/LOG(p2eff/Alam**2)))
         IF ( Iset.EQ.0 ) THEN
            xbot = xsea*(1.-(sbt/sll)**2)
         ELSE
            xbot = MAX(0.,xsea-xsea0*x1**(2.667*s))*(1.-sbt/sll)
         END IF
      END IF
 
C...Fill parton distributions.
      Xpga(0) = xglu
      Xpga(1) = xsea
      Xpga(2) = xsea
      Xpga(3) = xsea
      Xpga(4) = xchm
      Xpga(5) = xbot
      Xpga(kfa) = Xpga(kfa) + xval
      DO kfl = 1 , 5
         Xpga(-kfl) = Xpga(kfl)
      END DO
      Vxpga(kfa) = xval
      Vxpga(-kfa) = xval
 
      END SUBROUTINE
