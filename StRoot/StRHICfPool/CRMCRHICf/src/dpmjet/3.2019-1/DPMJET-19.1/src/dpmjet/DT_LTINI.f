
      SUBROUTINE DT_LTINI(Idpr,Idta,Epn0,Ppn0,Ecm0,Mode)
 
C***********************************************************************
C Initializations of Lorentz-transformations, calculation of Lorentz-  *
C parameters.                                                          *
C This version dated 13.11.95 is written by  S. Roesler.               *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION amgam , amgam2 , amlpt2 , amp , amp2 , amt , 
     &                 amt2 , ecm , Ecm0 , epn , Epn0 , etarg , ONE , 
     &                 ppn , Ppn0 , ptarg , q2 , s , TINY10 , TINY3
      DOUBLE PRECISION TWO , ZERO
      INTEGER idp , Idpr , idt , Idta , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY3=1.0D-3,ZERO=0.0D0,ONE=1.0D0,
     &           TWO=2.0D0)
 
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
 
      q2 = VIRt
      idp = Idpr
      IF ( MCGene.NE.3 ) THEN
C lepton-projectiles and PHOJET: initialize real photon instead
         IF ( (Idpr.EQ.3) .OR. (Idpr.EQ.4) .OR. (Idpr.EQ.10) .OR. 
     &        (Idpr.EQ.11) .OR. (Idpr.EQ.5) .OR. (Idpr.EQ.6) ) THEN
            idp = 7
            q2 = ZERO
         END IF
      END IF
      idt = Idta
      epn = Epn0
      ppn = Ppn0
      ecm = Ecm0
      amp = AAM(idp) - SQRT(ABS(q2))
      amt = AAM(idt)
      amp2 = SIGN(amp**2,amp)
      amt2 = amt**2
      IF ( Ecm0.GT.ZERO ) THEN
         epn = (ecm**2-amp2-amt2)/(TWO*amt)
         IF ( amp2.GT.ZERO ) THEN
            ppn = SQRT((epn+amp)*(epn-amp))
         ELSE
            ppn = SQRT(epn**2-amp2)
         END IF
      ELSE
         IF ( (Epn0.NE.ZERO) .AND. (Ppn0.EQ.ZERO) ) THEN
            IF ( idp.EQ.7 ) epn = ABS(epn)
            IF ( epn.LT.ZERO ) epn = ABS(epn) + amp
            IF ( amp2.GT.ZERO ) THEN
               ppn = SQRT((epn+amp)*(epn-amp))
            ELSE
               ppn = SQRT(epn**2-amp2)
            END IF
         ELSE IF ( (Ppn0.GT.ZERO) .AND. (Epn0.EQ.ZERO) ) THEN
            IF ( amp2.GT.ZERO ) THEN
               epn = ppn*SQRT(ONE+(amp/ppn)**2)
            ELSE
               epn = SQRT(ppn**2+amp2)
            END IF
         END IF
         ecm = SQRT(amp2+amt2+TWO*amt*epn)
      END IF
      UMO = ecm
      EPRoj = epn
      PPRoj = ppn
      IF ( amp2.GT.ZERO ) THEN
         etarg = (ecm**2-amp2-amt2)/(TWO*amp)
         ptarg = -SQRT((etarg+amt)*(etarg-amt))
      ELSE
         etarg = TINY10
         ptarg = TINY10
      END IF
C photon-projectiles (get momentum in cm-frame for virtuality Q^2)
      IF ( idp.EQ.7 ) THEN
         PGAmm(1) = ZERO
         PGAmm(2) = ZERO
         amgam = amp
         amgam2 = amp2
         IF ( Ecm0.GT.ZERO ) THEN
            s = Ecm0**2
         ELSE IF ( (Epn0.NE.ZERO) .AND. (Ppn0.EQ.ZERO) ) THEN
            s = amgam2 + amt2 + TWO*amt*ABS(Epn0)
         ELSE IF ( (Ppn0.GT.ZERO) .AND. (Epn0.EQ.ZERO) ) THEN
            s = amgam2 + amt2 + TWO*amt*SQRT(Ppn0**2+amgam2)
         END IF
         PGAmm(3) = SQRT((s**2-TWO*amgam2*s-TWO*amt2*s-TWO*amgam2*amt2+
     &              amgam2**2+amt2**2)/(4.0D0*s))
         PGAmm(4) = SQRT(amgam2+PGAmm(3)**2)
         IF ( Mode.EQ.1 ) THEN
            PNUcl(1) = ZERO
            PNUcl(2) = ZERO
            PNUcl(3) = -PGAmm(3)
            PNUcl(4) = SQRT(s) - PGAmm(4)
         END IF
      END IF
      IF ( (Idpr.EQ.3) .OR. (Idpr.EQ.4) .OR. (Idpr.EQ.10) .OR. 
     &     (Idpr.EQ.11) ) THEN
         PLEpt0(1) = ZERO
         PLEpt0(2) = ZERO
C neglect lepton masses
C        AMLPT2   = AAM(IDPR)**2
         amlpt2 = ZERO
C
         IF ( Ecm0.GT.ZERO ) THEN
            s = Ecm0**2
         ELSE IF ( (Epn0.NE.ZERO) .AND. (Ppn0.EQ.ZERO) ) THEN
            s = amlpt2 + amt2 + TWO*amt*ABS(Epn0)
         ELSE IF ( (Ppn0.GT.ZERO) .AND. (Epn0.EQ.ZERO) ) THEN
            s = amlpt2 + amt2 + TWO*amt*SQRT(Ppn0**2+amlpt2)
         END IF
         PLEpt0(3) = SQRT((s**2-TWO*amlpt2*s-TWO*amt2*s-TWO*amlpt2*amt2+
     &               amlpt2**2+amt2**2)/(4.0D0*s))
         PLEpt0(4) = SQRT(amlpt2+PLEpt0(3)**2)
         PNUcl(1) = ZERO
         PNUcl(2) = ZERO
         PNUcl(3) = -PLEpt0(3)
         PNUcl(4) = SQRT(s) - PLEpt0(4)
      END IF
C Lorentz-parameter for transformation Lab. - projectile rest system
      IF ( (idp.EQ.7) .OR. (amp.LT.TINY10) ) THEN
         GALab = TINY10
         BGLab = TINY10
         BLAb = TINY10
      ELSE
         GALab = EPRoj/amp
         BGLab = PPRoj/amp
         BLAb = BGLab/GALab
      END IF
C Lorentz-parameter for transf. proj. rest sys. - nucl.-nucl. cms.
      IF ( idp.EQ.7 ) THEN
         GACms(1) = TINY10
         BGCms(1) = TINY10
      ELSE
         GACms(1) = (etarg+amp)/UMO
         BGCms(1) = ptarg/UMO
      END IF
C Lorentz-parameter for transformation Lab. - nucl.-nucl. cms.
      GACms(2) = (EPRoj+amt)/UMO
      BGCms(2) = PPRoj/UMO
      PPCm = GACms(2)*PPRoj - BGCms(2)*EPRoj
 
      Epn0 = epn
      Ppn0 = ppn
      Ecm0 = ecm
 
      END SUBROUTINE
