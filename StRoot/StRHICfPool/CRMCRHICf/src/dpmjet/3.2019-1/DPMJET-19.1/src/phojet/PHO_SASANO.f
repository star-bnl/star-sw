
      SUBROUTINE PHO_SASANO(Kf,X,Q2,P2,Alam,Xpga,Vxpga)
      IMPLICIT NONE
      REAL aem , aem2pi , Alam , alamsq , chsq , fac , P2 , p2eff , 
     &     pmb , pmc , Q2 , q2div , q2eff , s , sbt , sch , sll , snf3 , 
     &     snf4 , snfp
      REAL snfq , tdiff , Vxpga , X , xbot , xchm , xglu , xl , Xpga , 
     &     xsea , xval
      INTEGER Kf , kfa , kfl , kflmn , kflmx , nfp , nfq
C...Purpose: to evaluate the parton distributions of the anomalous
C...photon, inhomogeneously evolved from a scale P2 (where it vanishes)
C...to Q2.
C...KF=0 gives the sum over (up to) 5 flavours,
C...KF<0 limits to flavours up to abs(KF),
C...KF>0 is for flavour KF only.
C...ALAM is the 4-flavour Lambda, which is automatically converted
C...to 3- and 5-flavour equivalents as needed.
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      DIMENSION Xpga(-6:6) , Vxpga(-6:6) , alamsq(3:5)
      DATA pmc/1.3/ , pmb/4.6/ , aem/0.007297/ , aem2pi/0.0011614/
 
C...Reset output.
      DO kfl = -6 , 6
         Xpga(kfl) = 0.
         Vxpga(kfl) = 0.
      END DO
      IF ( Q2.LE.P2 ) RETURN
      kfa = ABS(Kf)
 
C...Calculate Lambda; protect against unphysical Q2 and P2 input.
      alamsq(3) = (Alam*(pmc/Alam)**(2./27.))**2
      alamsq(4) = Alam**2
      alamsq(5) = (Alam*(Alam/pmb)**(2./23.))**2
      p2eff = MAX(P2,1.2*alamsq(3))
      IF ( Kf.EQ.4 ) p2eff = MAX(p2eff,pmc**2)
      IF ( Kf.EQ.5 ) p2eff = MAX(p2eff,pmb**2)
      q2eff = MAX(Q2,p2eff)
      xl = -LOG(X)
 
C...Find number of flavours at lower and upper scale.
      nfp = 4
      IF ( p2eff.LT.pmc**2 ) nfp = 3
      IF ( p2eff.GT.pmb**2 ) nfp = 5
      nfq = 4
      IF ( q2eff.LT.pmc**2 ) nfq = 3
      IF ( q2eff.GT.pmb**2 ) nfq = 5
 
C...Define range of flavour loop.
      IF ( Kf.EQ.0 ) THEN
         kflmn = 1
         kflmx = 5
      ELSE IF ( Kf.LT.0 ) THEN
         kflmn = 1
         kflmx = kfa
      ELSE
         kflmn = kfa
         kflmx = kfa
      END IF
 
C...Loop over flavours the photon can branch into.
      DO kfl = kflmn , kflmx
 
C...Light flavours: calculate t range and (approximate) s range.
         IF ( kfl.LE.3 .AND. (kfl.EQ.1 .OR. kfl.EQ.Kf) ) THEN
            tdiff = LOG(q2eff/p2eff)
            s = (6./(33.-2.*nfq))
     &          *LOG(LOG(q2eff/alamsq(nfq))/LOG(p2eff/alamsq(nfq)))
            IF ( nfq.GT.nfp ) THEN
               q2div = pmb**2
               IF ( nfq.EQ.4 ) q2div = pmc**2
               snfq = (6./(33.-2.*nfq))
     &                *LOG(LOG(q2div/alamsq(nfq))/LOG(p2eff/alamsq(nfq))
     &                )
               snfp = (6./(33.-2.*(nfq-1)))
     &                *LOG(LOG(q2div/alamsq(nfq-1))/LOG
     &                (p2eff/alamsq(nfq-1)))
               s = s + (LOG(q2div/p2eff)/LOG(q2eff/p2eff))*(snfp-snfq)
            END IF
            IF ( nfq.EQ.5 .AND. nfp.EQ.3 ) THEN
               q2div = pmc**2
               snf4 = (6./(33.-2.*4))
     &                *LOG(LOG(q2div/alamsq(4))/LOG(p2eff/alamsq(4)))
               snf3 = (6./(33.-2.*3))
     &                *LOG(LOG(q2div/alamsq(3))/LOG(p2eff/alamsq(3)))
               s = s + (LOG(q2div/p2eff)/LOG(q2eff/p2eff))*(snf3-snf4)
            END IF
 
C...u and s quark do not need a separate treatment when d has been done.
         ELSE IF ( kfl.EQ.2 .OR. kfl.EQ.3 ) THEN
 
C...Charm: as above, but only include range above c threshold.
         ELSE IF ( kfl.EQ.4 ) THEN
            IF ( Q2.LE.pmc**2 ) GOTO 100
            p2eff = MAX(p2eff,pmc**2)
            q2eff = MAX(q2eff,p2eff)
            tdiff = LOG(q2eff/p2eff)
            s = (6./(33.-2.*nfq))
     &          *LOG(LOG(q2eff/alamsq(nfq))/LOG(p2eff/alamsq(nfq)))
            IF ( nfq.EQ.5 .AND. nfp.EQ.4 ) THEN
               q2div = pmb**2
               snfq = (6./(33.-2.*nfq))
     &                *LOG(LOG(q2div/alamsq(nfq))/LOG(p2eff/alamsq(nfq))
     &                )
               snfp = (6./(33.-2.*(nfq-1)))
     &                *LOG(LOG(q2div/alamsq(nfq-1))/LOG
     &                (p2eff/alamsq(nfq-1)))
               s = s + (LOG(q2div/p2eff)/LOG(q2eff/p2eff))*(snfp-snfq)
            END IF
 
C...Bottom: as above, but only include range above b threshold.
         ELSE IF ( kfl.EQ.5 ) THEN
            IF ( Q2.LE.pmb**2 ) GOTO 100
            p2eff = MAX(p2eff,pmb**2)
            q2eff = MAX(Q2,p2eff)
            tdiff = LOG(q2eff/p2eff)
            s = (6./(33.-2.*nfq))
     &          *LOG(LOG(q2eff/alamsq(nfq))/LOG(p2eff/alamsq(nfq)))
         END IF
 
C...Evaluate flavour-dependent prefactor (charge^2 etc.).
         chsq = 1./9.
         IF ( kfl.EQ.2 .OR. kfl.EQ.4 ) chsq = 4./9.
         fac = aem2pi*2.*chsq*tdiff
 
C...Evaluate parton distributions (normalized to unit momentum sum).
         IF ( kfl.EQ.1 .OR. kfl.EQ.4 .OR. kfl.EQ.5 .OR. kfl.EQ.Kf ) THEN
            xval = ((1.5+2.49*s+26.9*s**2)/(1.+32.3*s**2)
     &             *X**2+(1.5-0.49*s+7.83*s**2)/(1.+7.68*s**2)*(1.-X)
     &             **2+1.5*s/(1.-3.2*s+7.*s**2)*X*(1.-X))
     &             *X**(1./(1.+0.58*s))*(1.-X**2)**(2.5*s/(1.+10.*s))
            xglu = 2.*s/(1.+4.*s+7.*s**2)*X**(-1.67*s/(1.+2.*s))
     &             *(1.-X**2)**(1.2*s)
     &             *((4.*X**2+7.*X+4.)*(1.-X)/3.-2.*X*(1.+X)*xl)
            xsea = 0.333*s**2/(1.+4.90*s+4.69*s**2+21.4*s**3)
     &             *X**(-1.18*s/(1.+1.22*s))*(1.-X)**(1.2*s)
     &             *((8.-73.*X+62.*X**2)*(1.-X)/9.+(3.-8.*X**2/3.)
     &             *X*xl+(2.*X-1.)*X*xl**2)
 
C...Threshold factors for c and b sea.
            sll = LOG(LOG(q2eff/Alam**2)/LOG(p2eff/Alam**2))
            xchm = 0.
            IF ( Q2.GT.pmc**2 .AND. Q2.GT.1.001*p2eff ) THEN
               sch = MAX(0.,LOG(LOG(pmc**2/Alam**2)/LOG(p2eff/Alam**2)))
               xchm = xsea*(1.-(sch/sll)**3)
            END IF
            xbot = 0.
            IF ( Q2.GT.pmb**2 .AND. Q2.GT.1.001*p2eff ) THEN
               sbt = MAX(0.,LOG(LOG(pmb**2/Alam**2)/LOG(p2eff/Alam**2)))
               xbot = xsea*(1.-(sbt/sll)**3)
            END IF
         END IF
 
C...Add contribution of each valence flavour.
         Xpga(0) = Xpga(0) + fac*xglu
         Xpga(1) = Xpga(1) + fac*xsea
         Xpga(2) = Xpga(2) + fac*xsea
         Xpga(3) = Xpga(3) + fac*xsea
         Xpga(4) = Xpga(4) + fac*xchm
         Xpga(5) = Xpga(5) + fac*xbot
         Xpga(kfl) = Xpga(kfl) + fac*xval
         Vxpga(kfl) = Vxpga(kfl) + fac*xval
 100  END DO
      DO kfl = 1 , 5
         Xpga(-kfl) = Xpga(kfl)
         Vxpga(-kfl) = Vxpga(kfl)
      END DO
 
      END SUBROUTINE
