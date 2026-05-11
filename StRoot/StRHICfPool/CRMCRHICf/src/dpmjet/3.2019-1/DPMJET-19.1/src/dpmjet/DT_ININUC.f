
      SUBROUTINE DT_ININUC(Id,Nmass,Nch,Coord,Js,Imode)
 
C***********************************************************************
C Samples initial configuration of nucleons in nucleus with mass NMASS *
C including Fermi-momenta (if reqested).                               *
C          ID             BAMJET-code for hadrons (instead of nuclei)  *
C          NMASS          mass number of nucleus (number of nucleons)  *
C          NCH            charge of nucleus                            *
C          COORD(3,NMASS) coordinates of nucleons inside nucleus in fm *
C          JS(NMASS) > 0  nucleon undergoes nucleon-nucleon interact.  *
C          IMODE = 1      projectile nucleus                           *
C                = 2      target     nucleus                           *
C                = 3      target     nucleus (E_lab<E_thr for HADRIN)  *
C Adopted from a part of the old KKEVT routine which was written by    *
C J. Ranft/H.-J.Moehring.                                              *
C This version dated 13.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Coord , DT_RNDM , FM2MM , frac , pbin , pf , 
     &                 pfer , pftot , rr
      INTEGER i , Id , IDT_IPDGHA , idx , Imode , Js , k , MAXINT , 
     &        MAXNCL , MAXSQU , MAXVQU , mode , nc , Nch , nhadri , 
     &        Nmass , nn , nneu , np
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (FM2MM=1.0D-12)
 
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C auxiliary common for chain system storage (DTUNUC 1.x)
      INCLUDE 'inc/dtchsy'
C nuclear potential
      INCLUDE 'inc/dtnpot'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C Glauber formalism: collision properties
      INCLUDE 'inc/dtglcp'
C flavors of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmf'
C interface HADRIN-DPM
      INCLUDE 'inc/hnthre'
 
      DIMENSION pf(4) , pftot(4) , Coord(3,MAXNCL) , Js(MAXNCL)
 
C number of neutrons
      nneu = Nmass - Nch
C initializations
      np = 0
      nn = 0
      DO k = 1 , 4
         pftot(k) = 0.0D0
      END DO
      mode = Imode
      IF ( Imode.GT.2 ) mode = 2
C*sr 29.5. new NPOINT(1)-definition
C     IF (IMODE.GE.2) NPOINT(1) = NHKK+1
C*
      nhadri = 0
      nc = NHKk
 
C get initial configuration
      DO i = 1 , Nmass
         NHKk = NHKk + 1
         IF ( Js(i).GT.0 ) THEN
            ISThkk(NHKk) = 10 + mode
            IF ( Imode.EQ.3 ) THEN
C   additional treatment if HADRIN-generator is requested
               nhadri = nhadri + 1
               IF ( nhadri.EQ.1 ) IDXta = NHKk
               IF ( nhadri.GT.1 ) ISThkk(NHKk) = 14
            END IF
         ELSE
            ISThkk(NHKk) = 12 + mode
         END IF
         IF ( Nmass.GE.2 ) THEN
C   treatment for nuclei
            frac = 1.0D0 - DBLE(Nch)/DBLE(Nmass)
            rr = DT_RNDM(frac)
            IF ( (rr.LT.frac) .AND. (nn.LT.nneu) ) THEN
               idx = 8
               nn = nn + 1
            ELSE IF ( (rr.GE.frac) .AND. (np.LT.Nch) ) THEN
               idx = 1
               np = np + 1
            ELSE IF ( nn.LT.nneu ) THEN
               idx = 8
               nn = nn + 1
            ELSE IF ( np.LT.Nch ) THEN
               idx = 1
               np = np + 1
            END IF
            IDHkk(NHKk) = IDT_IPDGHA(idx)
            IDBam(NHKk) = idx
            IF ( mode.EQ.1 ) THEN
               IPOsp(i) = NHKk
               KKProj(i) = idx
            ELSE
               IPOst(i) = NHKk
               KKTarg(i) = idx
            END IF
            IF ( idx.EQ.1 ) THEN
               pfer = PFErmp(mode)
               pbin = SQRT(2.0D0*MAX(EBIndp(mode),0.D0)*AAM(1))
            ELSE
               pfer = PFErmn(mode)
               pbin = SQRT(2.0D0*MAX(EBIndn(mode),0.D0)*AAM(8))
            END IF
            CALL DT_FER4M(pfer,pbin,pf(1),pf(2),pf(3),pf(4),idx)
            DO k = 1 , 4
               pftot(k) = pftot(k) + pf(k)
               PHKk(k,NHKk) = pf(k)
            END DO
            PHKk(5,NHKk) = AAM(idx)
         ELSE
C   treatment for hadrons
            IDHkk(NHKk) = IDT_IPDGHA(Id)
            IDBam(NHKk) = Id
            PHKk(4,NHKk) = AAM(Id)
            PHKk(5,NHKk) = AAM(Id)
C* VDM assumption
C            IF (IDHKK(NHKK).EQ.22) THEN
C               PHKK(4,NHKK) = AAM(33)
C               PHKK(5,NHKK) = AAM(33)
C            ENDIF
            IF ( mode.EQ.1 ) THEN
               IPOsp(i) = NHKk
               KKProj(i) = Id
               PHKk(5,NHKk) = PHKk(5,NHKk) - SQRT(VIRt)
            ELSE
               IPOst(i) = NHKk
               KKTarg(i) = Id
            END IF
         END IF
         DO k = 1 , 3
            VHKk(k,NHKk) = Coord(k,i)*FM2MM
            WHKk(k,NHKk) = Coord(k,i)*FM2MM
         END DO
         IF ( mode.EQ.2 ) VHKk(1,NHKk) = VHKk(1,NHKk) + BIMpac*FM2MM
         IF ( mode.EQ.2 ) WHKk(1,NHKk) = WHKk(1,NHKk) + BIMpac*FM2MM
         VHKk(4,NHKk) = 0.0D0
         WHKk(4,NHKk) = 0.0D0
      END DO
 
C balance Fermi-momenta
      IF ( Nmass.GE.2 ) THEN
         DO i = 1 , Nmass
            nc = nc + 1
            DO k = 1 , 3
               PHKk(k,nc) = PHKk(k,nc) - pftot(k)/DBLE(Nmass)
            END DO
            PHKk(4,nc) = SQRT(PHKk(5,nc)**2+PHKk(1,nc)**2+PHKk(2,nc)
     &                   **2+PHKk(3,nc)**2)
         END DO
      END IF
 
      END SUBROUTINE
