
      SUBROUTINE DT_DEFAUL(Epn,Ppn)
 
C***********************************************************************
C Variables are set to default values.                                 *
C This version dated 8.5.95 is written by S. Roesler.                  *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Epn , ONE , potmes , Ppn , TINY10 , TWOPI , ZERO
      INTEGER i , j
      SAVE 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY10=1.0D-10)
      PARAMETER (TWOPI=6.283185307179586454D+00)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C nuclear potential
      INCLUDE 'inc/dtnpot'
C interface HADRIN-DPM
      INCLUDE 'inc/hnthre'
C central particle production, impact parameter biasing
      INCLUDE 'inc/dtimpa'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C parameter for intranuclear cascade
      INCLUDE 'inc/dtfoti'
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
C threshold values for x-sampling (DTUNUC 1.x)
      INCLUDE 'inc/dtxcut'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C n-n cross section fluctuations
      INCLUDE 'inc/dtxsfl'
C flags for particle decays
      INCLUDE 'inc/dtfrpa'
C diquark-breaking mechanism
      INCLUDE 'inc/dtdiqb'
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
C VDM parameter for photon-nucleus interactions
      INCLUDE 'inc/dtvdmp'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
C kinematical cuts for lepton-nucleus interactions
      INCLUDE 'inc/dtlcut'
C flags for activated histograms
      INCLUDE 'inc/dthis3'
C cuts for variable energy runs
      INCLUDE 'inc/dtvare'
C parameters for hA-diffraction
      INCLUDE 'inc/dtdiha'
C LEPTO
      INCLUDE 'inc/leptoi'
C steering flags for qel neutrino scattering modules
      INCLUDE 'inc/qneuto'
C event flag
      INCLUDE 'inc/dtevno'
 
      DATA potmes/0.002D0/
 
C common /DTNPOT/
      DO i = 1 , 2
         PFErmp(i) = ZERO
         PFErmn(i) = ZERO
         EBIndp(i) = ZERO
         EBIndn(i) = ZERO
         DO j = 1 , 210
            EPOt(i,j) = ZERO
         END DO
C nucleus independent meson potential
         EPOt(i,13) = potmes
         EPOt(i,14) = potmes
         EPOt(i,15) = potmes
         EPOt(i,16) = potmes
         EPOt(i,23) = potmes
         EPOt(i,24) = potmes
         EPOt(i,25) = potmes
      END DO
      FERmod = 0.55D0
      ETAcou(1) = ZERO
      ETAcou(2) = ZERO
      ICOul = 1
      LFErmi = .TRUE.
 
C common /HNTHRE/
      EHAdth = -99.0D0
      EHAdlo = 4.06D0
      EHAdhi = 6.0D0
      INThad = 1
      IDXta = 2
 
C common /DTIMPA/
      ICEntr = 0
      BIMin = ZERO
      BIMax = 1.0D10
      XSFrac = 1.0D0
 
C** anfe nuclear remnant
C      ICENTR = -2
C      BIMIN  = ZERO
C      BIMAX  = 1.0D8
C      XSFRAC = 1.0D0
 
C common /DTPRTA/
      IP = 1
      IPZ = 1
      IT = 1
      ITZ = 1
      IJProj = 1
      IBProj = 1
      IJTarg = 1
      IBTarg = 1
C common /DTGPRO/
      VIRt = ZERO
      DO i = 1 , 4
         PGAmm(i) = ZERO
         PLEpt0(i) = ZERO
         PLEpt1(i) = ZERO
         PNUcl(i) = ZERO
      END DO
      IDIrec = 0
 
C common /DTFOTI/
C*sr 7.4.98: changed after corrected B-sampling
C     TAUFOR = 4.4D0
      TAUfor = 3.5D0
      KTAuge = 25
      ITAuve = 1
      INCmod = 1
      LPAuli = .TRUE.
 
C common /DTCHAI/
      SEAsq = ONE
      MKCron = 1
      CROnco = 0.64D0
      ISIcha = 0
      CUTof = 100.0D0
      LCO2cr = .FALSE.
      IREcom = 1
      LINtpt = .TRUE.
 
C common /DTXCUT/
C  definition of soft quark distributions
      XSEacu = 0.05D0
      UNOn = 2.0D0
      UNOm = 1.5D0
      UNOsea = 5.0D0
C  cutoff parameters for x-sampling
      CVQ = 1.0D0
      CDQ = 2.0D0
C     CSEA   = 0.3D0
      CSEa = 0.1D0
      SSMima = 1.2D0
      SSMimq = SSMima**2
      VVMthr = 2.0D0
 
C common /DTXSFL/
      IFLuct = 0
 
C common /DTFRPA/
      PDB = 0.15D0
      PDBsea(1) = 0.0D0
      PDBsea(2) = 0.0D0
      PDBsea(3) = 0.0D0
      ISIg0 = 0
      IPI0 = 0
      NMStu = 0
      NPAru = 0
      NMStj = 0
      NPArj = 0
 
C common /DTDIQB/
      DO i = 1 , 8
         DBRkr(1,i) = 5.0D0
         DBRkr(2,i) = 5.0D0
         DBRkr(3,i) = 10.0D0
         DBRka(1,i) = ZERO
         DBRka(2,i) = ZERO
         DBRka(3,i) = ZERO
      END DO
      CHAm1 = 0.2D0
      CHAm3 = 0.5D0
      CHAb1 = 0.7D0
      CHAb3 = 1.0D0
 
C common /DTFLG3/
      ISIngd = 0
      IDOubd = 0
      IFLagd = 0
      IDIff = 0
 
C common /DTMODL/
      MCGene = 2
      CMOdel(1) = 'DTUNUC  '
      CMOdel(2) = 'PHOJET  '
      CMOdel(3) = 'LEPTO   '
      CMOdel(4) = 'QNEUTRIN'
      LPHoin = .TRUE.
      ELOjet = 5.0D0
 
C common /DTLCUT/
      ECMin = 3.5D0
      ECMax = 1.0D10
      XBJmin = ZERO
      ELMin = ZERO
      EGMin = ZERO
      EGMax = 1.0D10
      YMIn = TINY10
      YMAx = 0.999D0
      Q2Min = TINY10
      Q2Max = 10.0D0
      THMin = ZERO
      THMax = TWOPI
      Q2Li = ZERO
      Q2Hi = 1.0D10
      ECMli = ZERO
      ECMhi = 1.0D10
 
C common /DTVDMP/
      RL2 = 2.0D0
      INTrge(1) = 1
      INTrge(2) = 3
      IDPdf = 2212
      MODega = 4
      ISHad(1) = 1
      ISHad(2) = 1
      ISHad(3) = 1
      EPSpol = ZERO
 
C common /DTGLGP/
      JSTatb = 1000
      JBInsb = 49
#ifndef FOR_CORSIKA
      CGLb = '        '
#else
cdh   do not overwrite the glauber data set name of corsika
#endif
      IF ( ITRspt.EQ.1 ) THEN
         IOGlb = 100
      ELSE
         IOGlb = 0
      END IF
      LPRod = .TRUE.
 
C common /DTHIS3/
      DO i = 1 , 50
         IHIspp(i) = 0
         IHIsxs(i) = 0
      END DO
      IXStbl = 0
 
C common /DTVARE/
      VARelo = ZERO
      VARehi = ZERO
      VARclo = ZERO
      VARchi = ZERO
 
C common /DTDIHA/
      DIBeta = -1.0D0
      DIAlph = ZERO
 
C common /LEPTOI/
      RPPn = 0.0
      LEPin = 0
      INTer = 0
 
C common /QNEUTO/
      NEUtyp = 1
      NEUdec = 0
 
C common /DTEVNO/
      NEVent = 1
      IF ( ITRspt.EQ.1 ) THEN
         ICAsca = 1
      ELSE
         ICAsca = 0
      END IF
 
C default Lab.-energy
      Epn = 200.0D0
      Ppn = SQRT((Epn-AAM(IJProj))*(Epn+AAM(IJProj)))
 
      END SUBROUTINE
