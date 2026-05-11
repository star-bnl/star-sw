
      SUBROUTINE DT_SHMAKI(Na,Nca,Nb,Ncb,Ijp,Ppn,Mode)
 
C***********************************************************************
C Initialisation of Glauber formalism. This subroutine has to be       *
C called once (in case of target emulsions as often as many different  *
C target nuclei are considered) before events are sampled.             *
C         NA / NCA   mass number/charge of projectile nucleus          *
C         NB / NCB   mass number/charge of target     nucleus          *
C         IJP        identity of projectile (hadrons/leptons/photons)  *
C         PPN        projectile momentum (for projectile nuclei:       *
C                    momentum per nucleon) in target rest system       *
C         MODE = 0   Glauber formalism invoked                         *
C              = 1   fitted results are loaded from data-file          *
C              = 99  NTARG is forced to be 1                           *
C                    (used in connection with GLAUBERI-card only)      *
C This version dated 22.03.96 is based on the original SHMAKI-routine  *
C and revised by S. Roesler.                                           *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION aecmhi , aecmlo , amp , amp2 , amt , amt2 , 
     &                 aq2hi , aq2lo , daecm , daq2 , ecm , elab , ONE , 
     &                 plab , Ppn , THREE , TINY10 , TWO , ZERO
      INTEGER i , iback , ibin , icout , iestep , Ijp , iqstep , 
     &        iveout , j , Mode , Na , Nb , Nca , Ncb , nidx , ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0,
     &           THREE=3.0D0)
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C kinematical cuts for lepton-nucleus interactions
      INCLUDE 'inc/dtlcut'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
C cuts for variable energy runs
      INCLUDE 'inc/dtvare'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
 
      DATA ntarg , icout , iveout/0 , 0 , 0/
 
C     CALL DT_HISHAD
C     STOP
 
      ntarg = ntarg + 1
      IF ( Mode.EQ.99 ) ntarg = 1
      nidx = -ntarg
      IF ( Mode.EQ.-1 ) nidx = ntarg
 
      IF ( (icout.LT.15) .AND. (MCGene.NE.4) ) icout = icout + 1
 
      IF ( LPRi.GT.4 .AND. icout.EQ.1 ) WRITE (LOUt,99010)
99010 FORMAT (//,1X,'SHMAKI:    Glauber formalism (Shmakov et. al) -',
     &        ' initialization',/,12X,'--------------------------',
     &        '-------------------------',/)
 
      IF ( Mode.EQ.2 ) THEN
         CALL DT_XSGLAU(Na,Nb,Ijp,ZERO,VIRt,UMO,1,1,nidx)
         CALL DT_SHFAST(Mode,Ppn,iback)
         STOP ' Glauber pre-initialization done'
      END IF
      IF ( Mode.EQ.1 ) THEN
         CALL DT_PROFBI(Na,Nb,Ppn,ntarg)
      ELSE
         iback = 1
         IF ( Mode.EQ.3 ) CALL DT_SHFAST(Mode,Ppn,iback)
         IF ( iback.EQ.1 ) THEN
C lepton-nucleus (variable energy runs)
            IF ( (Ijp.EQ.3) .OR. (Ijp.EQ.4) .OR. (Ijp.EQ.10) .OR. 
     &           (Ijp.EQ.11) ) THEN
 
               IF ( (icout.LT.15) .AND. (MCGene.NE.4) .AND. LPRi.GT.4 )
     &              WRITE (LOUt,99020) Nb , Ncb
99020          FORMAT (1X,'variable energy run:     projectile-id:  7',
     &                 '    target A/Z: ',I3,' /',I3,/,/,8X,
     &                 'E_cm (GeV)    Q^2 (GeV^2)',
     &                 '    Sigma_tot (mb)     Sigma_in (mb)',/,7X,
     &                 '--------------------------------',
     &                 '------------------------------')
               aecmlo = LOG10(MIN(UMO,ECMli))
               aecmhi = LOG10(MIN(UMO,ECMhi))
               iestep = NEB - 1
               daecm = (aecmhi-aecmlo)/DBLE(iestep)
               IF ( aecmlo.EQ.aecmhi ) iestep = 0
               DO i = 1 , iestep + 1
                  ecm = 10.0D0**(aecmlo+DBLE(i-1)*daecm)
                  IF ( Q2Hi.GT.0.1D0 ) THEN
                     IF ( Q2Li.LT.0.01D0 ) THEN
                        CALL DT_XSGLAU(Na,Nb,7,ZERO,ZERO,ecm,i,1,nidx)
 
                        IF ( (icout.LT.15) .AND. (MCGene.NE.4) .AND. 
     &                       LPRi.GT.4 ) WRITE (LOUt,99070) ECMnn(i) , 
     &                       ZERO , XSTot(i,1,ntarg) , XSPro(i,1,ntarg)
                        Q2Li = 0.01D0
                        ibin = 2
                     ELSE
                        ibin = 1
                     END IF
                     iqstep = NQB - ibin
                     aq2lo = LOG10(Q2Li)
                     aq2hi = LOG10(Q2Hi)
                     daq2 = (aq2hi-aq2lo)/MAX(DBLE(iqstep),ONE)
                     DO j = ibin , iqstep + ibin
                        Q2 = 10.0D0**(aq2lo+DBLE(j-ibin)*daq2)
                        CALL DT_XSGLAU(Na,Nb,7,ZERO,Q2,ecm,i,j,nidx)
 
                        IF ( (icout.LT.15) .AND. (MCGene.NE.4) .AND. 
     &                       LPRi.GT.4 ) WRITE (LOUt,99070) ECMnn(i) , 
     &                       Q2G(j) , XSTot(i,j,ntarg) , 
     &                       XSPro(i,j,ntarg)
                     END DO
                  ELSE
                     CALL DT_XSGLAU(Na,Nb,7,ZERO,ZERO,ecm,i,1,nidx)
 
                     IF ( (icout.LT.15) .AND. (MCGene.NE.4) .AND. 
     &                    LPRi.GT.4 ) WRITE (LOUt,99070) ECMnn(i) , 
     &                    ZERO , XSTot(i,1,ntarg) , XSPro(i,1,ntarg)
                  END IF
               END DO
               iveout = 1
C hadron/photon/nucleus-nucleus
            ELSE IF ( (ABS(VARehi).GT.ZERO) .AND. 
     &                (ABS(VARehi).GT.ABS(VARelo)) ) THEN
               IF ( (icout.LT.15) .AND. (MCGene.NE.4) ) THEN
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,99030) Na , Nb , Ncb
99030             FORMAT (1X,'variable energy run:    projectile-id:',
     &                    I3,'    target A/Z: ',I3,' /',I3,/)
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,99040)
99040             FORMAT ('  E_cm (GeV)  E_Lab (GeV)  sig_tot^pp (mb)',
     &                    '  Sigma_tot (mb)  Sigma_prod (mb)',/,
     &                    ' -------------------------------------',
     &                    '--------------------------------------')
               END IF
               aecmlo = LOG10(VARclo)
               aecmhi = LOG10(VARchi)
               iestep = NEB - 1
               daecm = (aecmhi-aecmlo)/DBLE(iestep)
               IF ( aecmlo.EQ.aecmhi ) iestep = 0
               DO i = 1 , iestep + 1
                  ecm = 10.0D0**(aecmlo+DBLE(i-1)*daecm)
                  amp = 0.938D0
                  amt = 0.938D0
                  amp2 = amp**2
                  amt2 = amt**2
                  elab = (ecm**2-amp2-amt2)/(TWO*amt)
                  plab = SQRT((elab+amp)*(elab-amp))
                  CALL DT_XSGLAU(Na,Nb,Ijp,ZERO,VIRt,ecm,i,1,nidx)
 
                  IF ( (icout.LT.15) .AND. (MCGene.NE.4) .AND. 
     &                 LPRi.GT.4 ) WRITE (LOUt,99050) ecm , plab , 
     &                 SIGsh , XSTot(i,1,ntarg) , XSPro(i,1,ntarg)
99050             FORMAT (1X,F9.1,1X,E11.3,1X,F12.2,8X,F10.3,8X,F8.3)
               END DO
               iveout = 1
            ELSE
               CALL DT_XSGLAU(Na,Nb,Ijp,ZERO,VIRt,UMO,1,1,nidx)
            END IF
         END IF
      END IF
 
      IF ( (icout.LT.15) .AND. (iveout.EQ.0) .AND. (MCGene.NE.4) .AND. 
     &     (IOGlb.NE.100) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99060) Na , Nca , Nb , Ncb , 
     &        ECMnn(1) , SIGsh*10.0D0 , ROSh , BSLope , NSIteb , 
     &        NSTatb , XSPro(1,1,ntarg)
99060    FORMAT (38X,'projectile','      target',/,1X,
     &           'Mass number / charge',17X,I3,' /',I3,6X,I3,' /',I3,/,
     &           /,1X,'Nucleon-nucleon c.m. energy',9X,F10.2,' GeV',/,/,
     &           1X,'Parameters of elastic scattering amplitude:',/,5X,
     &           'sigma =',F7.2,' mb',6X,'rho = ',F9.4,6X,'slope = ',
     &           F4.1,' GeV^-2',/,/,1X,'Number of b-steps',4X,I3,8X,
     &           'statistics at each b-step',4X,I5,/,/,1X,
     &           'Prod. cross section  ',5X,F10.4,' mb',/)
      END IF
99070 FORMAT (9X,F6.1,9X,F6.2,8X,F8.3,11X,F8.3)
 
      END SUBROUTINE
