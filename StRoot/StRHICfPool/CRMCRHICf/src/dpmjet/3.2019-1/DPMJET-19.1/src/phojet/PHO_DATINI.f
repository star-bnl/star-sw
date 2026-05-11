
      SUBROUTINE PHO_DATINI
C*********************************************************************
C
C     initialization of variables and switches
C
C*********************************************************************
      IMPLICIT NONE
      INTEGER i , k , m
      DOUBLE PRECISION twopim
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  some constants
      INCLUDE 'inc/pocons'
C  event debugging information
      INCLUDE 'inc/podebg'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
C  scale parameters for parton model calculations
      INCLUDE 'inc/pohscl'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  cut probability distribution
#ifndef FOR_CORSIKA
      INCLUDE 'inc/poprob'
#else
      INCLUDE 'inc/poprob50'
#endif
C  gamma-lepton or gamma-hadron vertex information
      INCLUDE 'inc/pofsrc'
C  photon flux kinematics and cuts
      INCLUDE 'inc/pofcut'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  general process information
      INCLUDE 'inc/poprcs'
C  parameters of the "simple" Vector Dominance Model
      INCLUDE 'inc/posvdm'
C  parameters for DGLAP backward evolution in ISR
      INCLUDE 'inc/podgl1'
C  particles created by initial state evolution
      INCLUDE 'inc/popisr'
C  names of hard scattering processes
      INCLUDE 'inc/pohpro'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  interpolation tables for hard cross section and MC selection weights
#ifndef FOR_CORSIKA
      INCLUDE 'inc/pohtab'
#else
      INCLUDE 'inc/pohtab50'
cdh  datadir for path to the data sets to be read in by dpmjet/phojet
      COMMON /DATADIR/ DATADIR
      CHARACTER*132    DATADIR
#endif
 
C  initialize /POCONS/
      PI = ATAN(1.D0)*4.D0
      PI2 = 2.D0*PI
      PI4 = 2.D0*PI2
C  GeV**-2 --> millibarn (multiply by GEV2MB to get mb as units)
      GEV2mb = 0.389365D0
C  precalculate quark charges
      DO i = 1 , 6
         Q_Ch(i) = DBLE(2-3*MOD(i,2))/3.D0
         Q_Ch(-i) = -Q_Ch(i)
 
         Q_Ch2(i) = Q_Ch(i)**2
         Q_Ch2(-i) = Q_Ch2(i)
 
         Q_Ch4(i) = Q_Ch2(i)**2
         Q_Ch4(-i) = Q_Ch4(i)
      END DO
      Q_Ch(0) = 0.D0
      Q_Ch2(0) = 0.D0
      Q_Ch4(0) = 0.D0
 
C  initialize /GLOCMS/
      ECM = 50.D0
      PMAss(1) = 0.D0
      PVIrt(1) = 0.D0
      PMAss(2) = 0.D0
      PVIrt(2) = 0.D0
      IFPap(1) = 22
      IFPap(2) = 22
C  initialize /HADVAL/
      IHFld(1,1) = 0
      IHFld(1,2) = 0
      IHFld(2,1) = 0
      IHFld(2,2) = 0
      IHFls(1) = 1
      IHFls(2) = 1
      XPSub = 1.D0
      XTSub = 1.D0
C  initialize /MODELS/
      ISWmdl(1) = 3
      MDLna(1) = 'AMPL MOD'
      ISWmdl(2) = 1
      MDLna(2) = 'MIN-BIAS'
      ISWmdl(3) = 1
      MDLna(3) = 'PTS DISH'
      ISWmdl(4) = 1
      MDLna(4) = 'PTS DISP'
      ISWmdl(5) = 2
      MDLna(5) = 'PTS ASSI'
      ISWmdl(6) = 3
      MDLna(6) = 'HADRONIZ'
      ISWmdl(7) = 2
      MDLna(7) = 'MASS COR'
      ISWmdl(8) = 3
      MDLna(8) = 'PAR SHOW'
      ISWmdl(9) = 0
      MDLna(9) = 'GLU SPLI'
      ISWmdl(10) = 2
      MDLna(10) = 'VIRT PHO'
      ISWmdl(11) = 0
      MDLna(11) = 'LARGE NC'
      ISWmdl(12) = 0
      MDLna(12) = 'LIPA POM'
      ISWmdl(13) = 1
      MDLna(13) = 'QELAS VM'
      ISWmdl(14) = 2
      MDLna(14) = 'ENHA GRA'
      ISWmdl(15) = 4
      MDLna(15) = 'MULT SCA'
      ISWmdl(16) = 4
      MDLna(16) = 'MULT DIF'
      ISWmdl(17) = 4
      MDLna(17) = 'MULT CDF'
      ISWmdl(18) = 0
      MDLna(18) = 'BALAN PT'
      ISWmdl(19) = 1
      MDLna(19) = 'POMV FLA'
      ISWmdl(20) = 0
      MDLna(20) = 'SEA  FLA'
      ISWmdl(21) = 2
      MDLna(21) = 'SPIN DEC'
      ISWmdl(22) = 1
      MDLna(22) = 'DIF.MASS'
      ISWmdl(23) = 1
      MDLna(23) = 'DIFF RES'
      ISWmdl(24) = 0
      MDLna(24) = 'PTS HPOM'
      ISWmdl(25) = 0
      MDLna(25) = 'POM CORR'
      ISWmdl(26) = 1
      MDLna(26) = 'OVERLAP '
      ISWmdl(27) = 0
      MDLna(27) = 'MUL R/AN'
      ISWmdl(28) = 1
      MDLna(28) = 'SUR PROB'
      ISWmdl(29) = 1
      MDLna(29) = 'PRIMO KT'
      ISWmdl(30) = 0
      MDLna(30) = 'DIFF. CS'
      ISWmdl(31) = -9999
C  mass-independent sea flavour ratios (for low-mass strings)
      PARmdl(1) = 0.425D0
      PARmdl(2) = 0.425D0
      PARmdl(3) = 0.15D0
      PARmdl(4) = 0.D0
      PARmdl(5) = 0.D0
      PARmdl(6) = 0.D0
C  suppression by energy momentum conservation
      PARmdl(8) = 9.D0
      PARmdl(9) = 7.D0
C  VDM factors
      PARmdl(10) = 0.866D0
      PARmdl(11) = 0.288D0
      PARmdl(12) = 0.288D0
      PARmdl(13) = 0.288D0
      PARmdl(14) = 0.866D0
      PARmdl(15) = 0.288D0
      PARmdl(16) = 0.288D0
      PARmdl(17) = 0.288D0
      PARmdl(18) = 0.D0
C  lower energy limit for initialization
      PARmdl(19) = 2.5D0
C  soft pt for hard scattering remnants
      PARmdl(20) = 5.D0
C  low energy beta of soft pt distribution 1
      PARmdl(21) = 4.5D0
C  high energy beta of soft pt distribution 1
      PARmdl(22) = 3.0D0
C  low energy beta of soft pt distribution 0
      PARmdl(23) = 2.5D0
C  high energy beta of soft pt distribution 0
      PARmdl(24) = 0.4D0
C  effective quark mass in photon wave function
      PARmdl(25) = 0.2D0
C  normalization of unevolved Pomeron PDFs
      PARmdl(26) = 0.3D0
C  effective VDM parameters for Q**2 dependence of cross section
      PARmdl(27) = 0.65D0
      PARmdl(28) = 0.08D0
      PARmdl(29) = 0.05D0
      PARmdl(30) = 0.22D0
      PARmdl(31) = 0.589824D0
      PARmdl(32) = 0.609961D0
      PARmdl(33) = 1.038361D0
      PARmdl(34) = 1.96D0
C  Q**2 suppression of multiple interactions
      PARmdl(35) = 0.59D0
C  pt cutoff defaults
      PARmdl(36) = 2.5D0
      PARmdl(37) = 2.5D0
      PARmdl(38) = 2.5D0
      PARmdl(39) = 2.5D0
C  enhancement factor for diffractive cross sections
      PARmdl(40) = 1.D0
      PARmdl(41) = 1.D0
      PARmdl(42) = 1.D0
C  mass in soft pt distribution
      PARmdl(43) = 0.D0
C  maximum of x allowed for leading particle
      PARmdl(44) = 0.9D0
C  max. mass sampled in diffraction
      PARmdl(45) = SQRT(0.4D0)
C  mass threshold in diffraction (2pi mass)
      PARmdl(46) = 0.3D0
C  regularization of slope parameter in diffraction
      PARmdl(47) = 4.D0
C  renormalized intercept for enhanced graphs
      PARmdl(48) = 1.08D0
C  coherence constraint for diff. cross sections
      PARmdl(49) = SQRT(0.05D0)
C  exponents of x distributions
C  baryon
      PARmdl(50) = 1.5D0
      PARmdl(51) = -0.5D0
      PARmdl(52) = -0.99D0
      PARmdl(53) = -0.99D0
C  meson (non-strangeness part)
      PARmdl(54) = -0.5D0
      PARmdl(55) = -0.5D0
      PARmdl(56) = -0.99D0
      PARmdl(57) = -0.99D0
C  meson (strangeness part)
      PARmdl(58) = -0.2D0
      PARmdl(59) = -0.2D0
      PARmdl(60) = -0.99D0
      PARmdl(61) = -0.99D0
C  particle remnant (no valence quarks)
      PARmdl(62) = -0.5D0
      PARmdl(63) = -0.5D0
      PARmdl(64) = -0.99D0
      PARmdl(65) = -0.99D0
C  ratio beetween triple-pomeron/reggeon couplings grrp/gppp
      PARmdl(66) = 10.D0
C  ratio beetween triple-pomeron/reggeon couplings gppr/gppp
      PARmdl(67) = 10.D0
C  min. abs(t) in diffraction
      PARmdl(68) = 0.D0
C  max. abs(t) in diffraction
      PARmdl(69) = 10.D0
C  min. mass for elastic pomerons in central diffraction
      PARmdl(70) = 2.D0
C  min. mass of diffractive blob in central diffraction
      PARmdl(71) = 2.D0
C  min. Feynman x cut in central diffraction
      PARmdl(72) = 0.D0
C  direct pomeron coupling
      PARmdl(74) = 0.D0
C  relative deviation allowed for energy-momentum conservation
C  energy-momentum relative deviation
      PARmdl(75) = 0.01D0
C  transverse momentum deviation
      PARmdl(76) = 0.01D0
C  couplings for unitarization in diffraction
C  non-unitarized pomeron coupling (sqrt(mb))
      PARmdl(77) = 3.D0
C  rescaling factor for pomeron PDF
      PARmdl(78) = 3.D0
C  coupling probabilities
      PARmdl(79) = 1.D0
      PARmdl(80) = 0.D0
C  scales to calculate alpha-s of matrix element
      PARmdl(81) = 1.D0
      PARmdl(82) = 1.D0
      PARmdl(83) = 1.D0
C  scales to calculate alpha-s of initial state radiation
      PARmdl(84) = 1.D0
      PARmdl(85) = 1.D0
      PARmdl(86) = 1.D0
C  scales to calculate alpha-s of final state radiation
      PARmdl(87) = 1.D0
      PARmdl(88) = 1.D0
      PARmdl(89) = 1.D0
C  scales to calculate PDFs
      PARmdl(90) = 1.D0
      PARmdl(91) = 1.D0
      PARmdl(92) = 1.D0
C  scale for ISR starting virtuality
      PARmdl(93) = 1.D0
C  min. virtuality to generate time-like showers in ISR
      PARmdl(94) = 2.D0
C  factor to scale the max. allowed time-like parton shower virtuality
      PARmdl(95) = 4.D0
C  max. transverse momentum to include inhard cross section
      PARmdl(96) = 900.D0
C  max. transverse momentum for primordial kt
      PARmdl(100) = 2.D0
C  weight factors for pt-distribution
      PARmdl(101) = 2.D0
      PARmdl(102) = 2.D0
      PARmdl(103) = 4.D0
      PARmdl(104) = 2.D0
      PARmdl(105) = 6.D0
      PARmdl(106) = 4.D0
C
C     PARMDL(110-125)  reserved for hard scattering
C  currently chosen scales for hard scattering
      DO i = 1 , 16
         PARmdl(109+i) = 0.D0
      END DO
C  virtuality cutoff in initial state evolution
      PARmdl(126) = PARmdl(36)**2
      PARmdl(127) = PARmdl(37)**2
      PARmdl(128) = PARmdl(38)**2
      PARmdl(129) = PARmdl(39)**2
C  virtuality cutoff for direct contribution to photon PDF
      PARmdl(130) = 1.D30
      PARmdl(131) = 1.D30
      PARmdl(132) = 1.D30
      PARmdl(133) = 1.D30
C  fraction of events without popcorn
      PARmdl(134) = -1.D0
C  fraction of diquarks with spin 1 (relative to sum of spin 1 and 0)
      PARmdl(135) = 0.5D0
C  soft color re-connection (fraction)
C  g g final state
      PARmdl(140) = 1.D0/64.D0
C  g q final state
      PARmdl(141) = 1.D0/24.D0
C  q q final state
      PARmdl(142) = 1.D0/9.D0
C  effective scale in Drees-Godbole like suppresion in photon PDF
      PARmdl(144) = 0.766D0**2
C  QCD scales (if PDF scales are not used, 4 active flavours)
      PARmdl(145) = 0.2D0**2
      PARmdl(146) = 0.2D0**2
      PARmdl(147) = 0.2D0**2
C  threshold scales for variable flavour calculation (GeV**2)
      PARmdl(148) = 1.3D0**2
      PARmdl(149) = 4.75D0**2
      PARmdl(150) = 172.D0**2
C  constituent quark masses
      PARmdl(151) = 0.3D0
      PARmdl(152) = 0.3D0
      PARmdl(153) = 0.5D0
      PARmdl(154) = 1.6D0
      PARmdl(155) = 5.D0
      PARmdl(156) = 174.D0
C  min. masses of valence quark
      PARmdl(157) = 0.3D0
C  min. masses of valence diquark
      PARmdl(158) = 0.8D0
C  min. mass of sea quark
      PARmdl(159) = 0.D0
C  suppression of strange quarks as photon valences
      PARmdl(160) = 0.2D0
C  min. masses for strings (used in PHO_SOFTXX)
      PARmdl(161) = 1.D0
      PARmdl(162) = 1.D0
      PARmdl(163) = 1.D0
      PARmdl(164) = 1.D0
C  min. momentum fraction for soft processes
      PARmdl(165) = 0.3D0
C  min. phase space for x-sampling
      PARmdl(166) = 0.135D0
C  Ross-Stodolsky exponent
      PARmdl(170) = 4.2D0
C  cutoff on photon-pomeron invariant mass in hadron-hadron collisions
      PARmdl(175) = 2.D0
 
C*sr
C  extra factor multiplying difference between Goulianos and PHOJET-
C  diff. cross sections
      PARmdl(200) = 0.6D0
C*
C**af  energy dependence of pt-cutoff in Golec-Biernat-Wusthoff model
      PARmdl(250) = 2.0D0
      PARmdl(251) = 400.D0
      PARmdl(252) = 200.D0
      PARmdl(253) = 0.19D0
C       PARMDL(250) = 1.6D0
C       PARMDL(251) = 515.D0
C       PARMDL(252) = 230.D0
C       PARMDL(253) = 0.20D0
C**af  alpomhp
      PARmdl(260) = 0.10D0
C     Regularization factor for PYTHIA style p_T0 hep-ph/0402078
      PARmdl(261) = 0.D0
C     Energy dependence of k-factor
      PARmdl(262) = 0.13D0
C  complex amplitudes, eikonal functions
      IPAmdl(1) = 0
C  allow for Reggeon cuts
      IPAmdl(2) = 1
C  decay of hadron resonances in diffraction (0 iso, 1 trans, 2 long)
      IPAmdl(3) = 0
C  polarization of photon resonances (0 none, 1 trans, 2 long)
      IPAmdl(4) = 1
C  pt of valence partons
      IPAmdl(5) = 1
C  pt of hard scattering remnant
      IPAmdl(6) = 2
C  energy-dependent cutoff for hard scattering
      IPAmdl(7) = 3
C  intercept used for the calculation of enhanced graphs
      IPAmdl(8) = 1
C  effective slope of hard scattering amplitde
      IPAmdl(9) = 3
C  mass dependence of slope parameters
      IPAmdl(10) = 0
C  lepton-photon vertex 1
      IPAmdl(11) = 0
C  lepton-photon vertex 2
      IPAmdl(12) = 0
C  call by DPMJET
      IPAmdl(13) = 0
C  method to sample x distributions
      IPAmdl(14) = 3
C  energy-momentum check
      IPAmdl(15) = 1
C  phase space correction for DPMJET interface
      IPAmdl(16) = 1
C  fragment strings from projectile/target/central diff. separately
      IPAmdl(17) = 1
C  method to construct strings for hard interactions
      IPAmdl(18) = 1
C  method to construct strings for soft sea (pomeron cuts)
      IPAmdl(19) = 0
C  method to construct strings in pomeron interactions
      IPAmdl(20) = 0
C  soft color re-connection
      IPAmdl(21) = 0
C  resummation of triple- and loop-Pomeron
      IPAmdl(24) = 1
C  resummation of X iterated triple-Pomeron
      IPAmdl(25) = 1
C  dimension of interpolation table for weights in hard scattering
      IPAmdl(30) = MAX_TAB_E
C  dimension of interpolation table for pomeron cut distribution
      IPAmdl(31) = IEETA1
C  number of cut soft pomerons (restriction by field dimension)
      IPAmdl(32) = IIMAX
C  number of cut hard pomerons (restriction by field dimension)
      IPAmdl(33) = KKMAX
C  tau pair production in direct photon-photon collisions
      IPAmdl(64) = 0
C  currently chosen scales for hard scattering
C  ATTENTION:   IPAMDL(65-80)  reserved for hard scattering!
      DO i = 1 , 16
         IPAmdl(64+i) = -99999
      END DO
C  scales to calculate alpha-s of matrix element
      IPAmdl(81) = 1
      IPAmdl(82) = 1
      IPAmdl(83) = 1
C  scales to calculate alpha-s of initial state radiation
      IPAmdl(84) = 1
      IPAmdl(85) = 1
      IPAmdl(86) = 1
C  scales to calculate alpha-s of final state radiation
      IPAmdl(87) = 1
      IPAmdl(88) = 1
      IPAmdl(89) = 1
C  scales to calculate PDFs
      IPAmdl(90) = 1
      IPAmdl(91) = 1
      IPAmdl(92) = 1
C  where to get the parameter sets from
      IPAmdl(99) = 1
C  program PHO_ABORT for fatal errors (simulation of division by zero)
      IPAmdl(100) = 0
C  initial state parton showers for all / hardest interaction(s)
      IPAmdl(101) = 1
C  final state parton showers for all / hardest interaction(s)
      IPAmdl(102) = 1
C  initial virtuality for ISR generation
      IPAmdl(109) = 1
C  qqbar-gamma coupling in initial state showers
      IPAmdl(110) = 1
C  generation of time-like showers during ISR
      IPAmdl(111) = 1
C  reweighting of multiple soft contributions for virtual photons
      IPAmdl(114) = 1
C  reweighting / use photon virtuality in photon PDF calculations
      IPAmdl(115) = 0
C  use full QPM model incl. interference terms (direct part in gam-gam)
      IPAmdl(116) = 0
C  matching sigma_tot to F2 as given by parton density at high Q2
      IPAmdl(117) = 1
C  use virtuality of target in F2 calculations (two-gamma only)
      IPAmdl(118) = 1
C  calculation of alpha_em
      IPAmdl(120) = 1
C  strict pt cutoff for gamma-gamma events
      IPAmdl(121) = 0
C  photon virtuality sampled in photon flux approximations
      IPAmdl(174) = 1
C  photon-pomeron: 0,1,2: both,left,right photon emission
      IPAmdl(175) = 0
C  keep full history information in PHOJET-JETSET interface
      IPAmdl(178) = 1
C  max. number of conservation law violations allowed in one run
      IPAmdl(179) = 20
C  selection of soft X values
C  max. iteration number in PHO_SELSXS
      IPAmdl(180) = 50
C  max. iteration number in PHO_SELSXR
      IPAmdl(181) = 200
C  max. iteration number in PHO_SELSX2
      IPAmdl(182) = 100
C  max. iteration number in PHO_SELSXI
      IPAmdl(183) = 50
 
C Default directory for data files
#ifndef FOR_CORSIKA
      IF ( LENdir.EQ.0 ) THEN
        DATDir = 'dpmdata/'
        LENDir = 8 
      END IF
#else
        LENDir = INDEX(DATADIR,' ')
        DATDir = DATADIR(1:LENDir-1)//'/'
#endif
C  Parameter file path
      IF ( INDEX(PARfn,'.dat').EQ.0 ) PARfn = 
     &   DATdir(1:LENdir)//'dpmjpar.dat'
 
C  initialize /PROBAB/
      IEEmax = IEETA1
      IMAx = IIMAX
      KMAx = KKMAX
 
      DO i = 1 , 30
         PARmdl(300+i) = -100000.D0
      END DO
C  initialize /POHDRN/
      QMAss(1) = PARmdl(151)
      QMAss(2) = PARmdl(152)
      QMAss(3) = PARmdl(153)
      QMAss(4) = PARmdl(154)
      QMAss(5) = PARmdl(155)
      QMAss(6) = PARmdl(156)
      BET = 8.D0
      PCOudi = 0.D0
      VALprg(1) = 1.D0
      VALprg(2) = 1.D0
C  number of light flavours (quarks treated as massless)
      NFS = 4
C  initialize /POCUT1/
      PTCut(1) = PARmdl(36)
      PTCut(2) = PARmdl(37)
      PTCut(3) = PARmdl(38)
      PTCut(4) = PARmdl(39)
      PSOmin = 0.D0
      XSOmin = 0.D0
C  initialize /POHAPA/
      NFBeta = 4
      NF = 4
      BQCd(1) = PI4/(11.D0-(2.D0/3.D0)*3)
      BQCd(2) = PI4/(11.D0-(2.D0/3.D0)*4)
      BQCd(3) = PI4/(11.D0-(2.D0/3.D0)*5)
      BQCd(4) = PI4/(11.D0-(2.D0/3.D0)*6)
C  initialize /POGAUP/
      NGAup1 = 12
      NGAup2 = 12
      NGAuet = 16
      NGAuin = 12
      NGAuso = 96
C  initialize //
      DO i = 1 , 100
         IF ( IDEb(i).EQ.0 ) IDEb(i) = 0
      END DO
C  initialize /PROCES/
      DO i = 1 , 11
         IPRon(i,1) = 1
      END DO
 
C  DPMJET default: no elastic scattering
      IPRon(2,1) = 0
 
      DO k = 2 , 4
         DO i = 2 , 11
            IPRon(i,k) = 0
         END DO
         IPRon(1,k) = 1
         IPRon(8,k) = 1
      END DO
C  initialize /POSVDM/
      twopim = 0.28D0
      RMIn(1) = 0.285D0
      RMIn(2) = 0.45D0
      RMIn(3) = 1.D0
      RMIn(4) = twopim
      VMAs(1) = 0.770D0
      VMAs(2) = 0.787D0
      VMAs(3) = 1.02D0
      VMAs(4) = twopim
      GAMm(1) = 0.155D0
      GAMm(2) = 0.01D0
      GAMm(3) = 0.0045D0
      GAMm(4) = 1.D0
      RMAx(1) = VMAs(1) + twopim
      RMAx(2) = VMAs(2) + twopim
      RMAx(3) = VMAs(3) + twopim
      RMAx(4) = VMAs(1) + twopim
      VMSl(1) = 11.D0
      VMSl(2) = 10.D0
      VMSl(3) = 6.D0
      VMSl(4) = 4.D0
      VMFa(1) = 0.0033D0
      VMFa(2) = 0.00036D0
      VMFa(3) = 0.0002D0
      VMFa(4) = 0.0002D0
C  initialize /PODGL1/
      Q2Misr(1) = PARmdl(36)**2
      Q2Misr(2) = PARmdl(36)**2
      PMIsr(1) = 1.D0
      PMIsr(2) = 1.D0
      ZMIsr(1) = 0.001D0
      ZMIsr(2) = 0.001D0
      AL2isr(1) = 0.046D0
      AL2isr(2) = 0.046D0
      NFSisr = 4
C  initialize /POPISR/
      DO i = 1 , 50
         IPOisr(1,2,i) = 0
         IPOisr(2,2,i) = 0
      END DO
C  initialize /POHPRO/
      PROc(0) = 'sum over processes'
      PROc(1) = 'G  +G  --> G  +G  '
      PROc(2) = 'Q  +QB --> G  +G  '
      PROc(3) = 'G  +Q  --> G  +Q  '
      PROc(4) = 'G  +G  --> Q  +QB '
      PROc(5) = 'Q  +QB --> Q  +QB '
      PROc(6) = 'Q  +QB --> QP +QBP'
      PROc(7) = 'Q  +Q  --> Q  +Q  '
      PROc(8) = 'Q  +QP --> Q  +QP '
      PROc(9) = 'resolved processes'
      PROc(10) = 'gam+Q  --> G  +Q  '
      PROc(11) = 'gam+G  --> Q  +QB '
      PROc(12) = 'Q  +gam--> G  +Q  '
      PROc(13) = 'G  +gam--> Q  +QB '
      PROc(14) = 'gam+gam--> Q  +QB '
      PROc(15) = 'direct processes  '
      PROc(16) = 'gam+gam--> l+ +l- '
 
C  initialize /POHRCS/
      DO m = 1 , MAX_PRO_2
         DO k = 1 , MPARMAX
            HWGx(m,k) = 0.D0
            HSIg(m,k) = 0.D0
            HDPt(m,k) = 0.D0
         END DO
      END DO
      DO i = 0 , 4
         DO k = 1 , MPARMAX
            DO m = -1 , MAX_PRO_2
C  switch all hard subprocesses on
               MH_pro_on(m,i,k) = 1
C  reset all counters
               MH_tried(m,i,k) = 0
               MH_acc_1(m,i,k) = 0
               MH_acc_2(m,i,k) = 0
            END DO
            MH_pro_on(16,i,k) = 0
         END DO
      END DO
 
C  initialize /POHTAB/
      DO i = 0 , 4
         DO k = 1 , MPARMAX
            IH_ecm_up(i,k) = 0
            IH_q2a_up(i,k) = 0
            IH_q2b_up(i,k) = 0
            HECm_tab(1,i,k) = 0.D0
         END DO
      END DO
      HECm_last = 0.D0
      IHA_last = 0
      IHB_last = 0
 
C  initialize /POFSRC/
      IGHel(1) = -1
      IGHel(2) = -1
C  initialize /LEPCUT/
      ECMin = 5.D0
      ECMax = 1.D+30
      EEMin1 = 1.D0
      EEMin2 = 1.D0
      YMAx1 = -1.D0
      YMAx2 = -1.D0
      THMin1 = 0.D0
      THMax1 = PI
      THMin2 = 0.D0
      THMax2 = PI
      ITAg1 = 1
      ITAg2 = 1
C  initialize /POWGHT/
      DO i = 1 , 20
         HSWcut(i) = 0.D0
         ISWcut(i) = 0
      END DO
      EVWght(1) = 1.D0
      IVWght(1) = 0
      SIGgen(1) = 0.D0
      SIGgen(2) = 0.D0
      SIGgen(3) = 0.D0
      SIGgen(4) = 0.D0
 
      END SUBROUTINE
