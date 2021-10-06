 
C*********************************************************************
 
C...PYTUNE
C...Presets for a few specific underlying-event and min-bias tunes
C...Note some tunes require external pdfs to be linked (e.g. 105:QW), 
C...others require particular versions of pythia (e.g. the SCI and GAL 
C...models). See below for details.
      SUBROUTINE PYTUNE(ITUNE) 
C
C ITUNE    NAME (detailed descriptions below)
C     0 Default : No settings changed => linked Pythia version's defaults.
C ====== Old UE, Q2-ordered showers ==========================================
C   100       A : Rick Field's Tune A 
C   101      AW : Rick Field's Tune AW
C   102      BW : Rick Field's Tune BW
C   103      DW : Rick Field's Tune DW
C   104     DWT : Rick Field's Tune DW with slower UE energy scaling
C   105      QW : Rick Field's Tune QW (NB: needs CTEQ6.1 pdfs externally)
C   106   ATLAS : Arthur Moraes' ATLAS tune
C   107     ACR : Tune A modified with annealing CR
C ====== New UE, Q2-ordered showers ==========================================
C   200    IM 1 : Intermediate model: new UE, Q2-ordered showers, annealing CR
C ====== New UE, interleaved pT-ordered showers, annealing CR ================
C   300      S0 : Sandhoff-Skands Tune 0 
C   301      S1 : Sandhoff-Skands Tune 1
C   302      S2 : Sandhoff-Skands Tune 2
C   303     S0A : S0 with "Tune A" UE energy scaling
C   304    NOCR : New UE "best try" without colour reconnections. 
C   305     Old : New UE, original (primitive) colour reconnections
C ======= The Uppsala models =================================================
C   ( NB! must be run with special modified Pythia 6.215 version )
C   ( available from http://www.isv.uu.se/thep/MC/scigal/        )
C   400   GAL 0 : Generalized area-law model. Old parameters.
C   401   SCI 0 : Soft-Colour-Interaction model. Old parameters.
C   402   GAL 1 : Generalized area-law model. Tevatron MB retuned.
C   403   SCI 1 : Soft-Colour-Interaction model. Tevatron MB retuned.
C
C More details;
C
C Quick Dictionary:
C      BE : Bose-Einstein
C      BR : Beam Remnants
C      CR : Colour Reconnections
C      HAD: Hadronization
C      ISR/FSR: Initial-State Radiation / Final-State Radiation
C      FSI: Final-State Interactions (=CR+BE)
C      MB : Minimum-bias
C      MI : Multiple Interactions
C      UE : Underlying Event 
C       
C   A (100) and AW (101). Old UE model, Q2-ordered showers.
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: extensively compared to CDF data (R.D. Field).
C...* Large starting scale for ISR (PARP(67)=4)
C...* AW has even more radiation due to smaller mu_R choice in alpha_s.
C...* See: http://www.phys.ufl.edu/~rfield/cdf/
C
C   BW (102). Old UE model, Q2-ordered showers.
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: extensively compared to CDF data (R.D. Field).
C...NB: Can also be run with Pythia 6.2 or 6.312+
C...* Small starting scale for ISR (PARP(67)=1)
C...* BW has more radiation due to smaller mu_R choice in alpha_s.
C...* See: http://www.phys.ufl.edu/~rfield/cdf/
C
C   DW (103) and DWT (104). Old UE model, Q2-ordered showers.
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: extensively compared to CDF data (R.D. Field).
C...NB: Can also be run with Pythia 6.2 or 6.312+
C...* Intermediate starting scale for ISR (PARP(67)=2.5)
C...* DWT has a different reference energy, the same as the "S" models
C...  below, leading to more UE activity at the LHC, but less at RHIC.
C...* See: http://www.phys.ufl.edu/~rfield/cdf/
C
C   QW (105). Old UE model, Q2-ordered showers.
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: uses CTEQ61 (external pdf library must be linked)
C
C   ATLAS (106). Old UE model, Q2-ordered showers.
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: tune used by the ATLAS collaboration.
C
C   ACR (107). Old UE model, Q2-ordered showers, annealing CR.
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.408+    ***
C...Key feature: Tune A modified to use annealing CR. 
C...NB: PARP(85)=0D0 and amount of CR is regulated by PARP(78).
C
C...IM1 (200). Intermediate model, Q2-ordered showers.
C...Key feature: new UE model with Q2-ordered showers and no interleaving.
C...* "Rap" tune of hep-ph/0402078, modified with new annealing CR.
C...* See: Sjostrand & Skands: JHEP 03(2004)053, hep-ph/0402078.
C
C   S0 (300) and S0A (303). New UE model, pT-ordered showers. 
C...Key feature: large amount of multiple interactions
C...* Somewhat faster than the other colour annealing scenarios.
C...* S0A has a faster energy scaling of the UE IR cutoff, borrowed 
C...  from Tune A, leading to less UE at the LHC, but more at RHIC.
C...* Small amount of radiation.
C...* Large amount of low-pT MI
C...* Low degree of proton lumpiness (broad matter dist.)
C...* CR Type S (driven by free triplets), of medium strength.
C...* See: Pythia6402 update notes or later.
C
C   S1 (301). New UE model, pT-ordered showers.
C...Key feature: large amount of radiation.
C...* Large amount of low-pT perturbative ISR
C...* Large amount of FSR off ISR partons
C...* Small amount of low-pT multiple interactions
C...* Moderate degree of proton lumpiness
C...* Least aggressive CR type (S+S Type I), but with large strength
C...* See: Sandhoff & Skands: FERMILAB-CONF-05-518-T, in hep-ph/0604120.
C
C   S2 (302). New UE model, pT-ordered showers. 
C...Key feature: very lumpy proton + gg string cluster formation allowed
C...* Small amount of radiation
C...* Moderate amount of low-pT MI
C...* High degree of proton lumpiness (more spiky matter distribution)
C...* Most aggressive CR type (S+S Type II), but with small strength
C...* See: Sandhoff & Skands: FERMILAB-CONF-05-518-T, in hep-ph/0604120.
C 
C   NOCR (304). New UE model, pT-ordered showers.
C...Key feature: no colour reconnections (NB: "Best fit" only).
C...* NB: <pT>(Nch) problematic in this tune.
C...* Small amount of radiation
C...* Small amount of low-pT MI
C...* Low degree of proton lumpiness
C...* Large BR composite x enhancement factor
C...* Most clever colour flow without CR ("Lambda ordering")
C
C...The GAL and SCI models (400+) are special and *SHOULD NOT* be run 
C...with an unmodified Pythia distribution. 
C...See http://www.isv.uu.se/thep/MC/scigal/ for more information.
C
C ::: + Future improvements?
C        Include also QCD K-factor a la M. Heinz / ATLAS TDR ?
C       (problem: K-factor affects everything so only works as
C        intended for min-bias, not for UE ... probably need a 
C        better long-term solution to handle UE as well. Anyway,
C        Mark uses MSTP(33) and PARP(31)-PARP(33).)

C...Global statements
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER PYK,PYCHGE,PYCOMP

C...Commonblocks.
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)

C...SCI and GAL Commonblocks
      COMMON /SCIPAR/MSWI(2),PARSCI(2)

C...Internal parameters      
      PARAMETER(MXTUNS=500)
      CHARACTER*8 CHVERS, CHDOC
      PARAMETER (CHVERS='1.000   ',CHDOC='Oct 2006')      
      CHARACTER*16 CHNAMS(0:MXTUNS), CHNAME
      CHARACTER*40 CHMSTJ(20), CHMSTP(51:100), CHPARP(61:100), 
     &    CHPARJ(41:100), CH40
      CHARACTER*60 CH60
      CHARACTER*70 CH70
      DATA (CHNAMS(I),I=0,1)/'Default',' '/
      DATA (CHNAMS(I),I=100,110)/
     &    'Tune A','Tune AW','Tune BW','Tune DW','Tune DWT','Tune QW',
     &    'ATLAS Tune','Tune ACR',3*' '/
      DATA (CHNAMS(I),I=300,310)/
     &    'Tune S0','Tune S1','Tune S2','Tune S0A','NOCR','Old',5*' '/
      DATA (CHNAMS(I),I=200,210)/
     &    'IM Tune 1',10*' '/
      DATA (CHNAMS(I),I=400,410)/
     &    'GAL Tune 0','SCI Tune 0','GAL Tune 1','SCI Tune 1',7*' '/
      DATA (CHMSTJ(I),I=11,20)/
     &    5*' ','HAD treatment of small-mass systems',4*' '/
      DATA (CHMSTP(I),I=51,100)/
     5    'PDF set','PDF set internal (=1) or pdflib (=2)',
     6    8*' ','ISR master switch',8*' ',
     7    'ISR IR regularization scheme',' ',
     7    'ISR scheme for FSR off ISR',8*' ',
     8    'UE model',
     8    'UE hadron transverse mass distribution',5*' ',
     8    'BR composite scheme','BR colour scheme',1*' ',
     9    'BR primordial kT distribution',
     9    'BR energy partitioning scheme',2*' ',
     9    'FSI colour (re-)connection model',5*' '/  
      DATA (CHPARP(I),I=61,100)/
     6    ' ','ISR IR cutoff',' ','ISR renormalization scale prefactor',
     6    2*' ','ISR Q2max factor',3*' ',
     7    'FSR Q2max factor for non-s-channel procs',5*' ', 
     7    'FSI colour reconnection turnoff scale',
     7    'FSI colour reconnection strength',
     7    'BR composite x enhancement','BR breakup suppression',
     8    2*'UE IR cutoff at reference ecm',
     8    2*'UE mass distribution parameter',
     8    'UE gg colour correlated fraction','UE total gg fraction',
     8    2*' ',
     8    'UE IR cutoff reference ecm','UE IR cutoff ecm scaling power',
     9    'BR primordial kT width <|kT|>',' ',
     9    'BR primordial kT UV cutoff',7*' '/    
      DATA (CHPARJ(I),I=41,90)/
     4    ' ','HAD string parameter b',8*' ',10*' ',10*' ',10*' ',
     8    'FSR Lambda_QCD scale','FSR IR cutoff',8*' '/    
      SAVE /PYDAT1/,/PYPARS/
      SAVE /SCIPAR/

C...1) Shorthand notation
      M13=MSTU(13)
      M11=MSTU(11)
      IF (ITUNE.LE.MXTUNS.AND.ITUNE.GE.0) THEN
        CHNAME=CHNAMS(ITUNE)
        IF (ITUNE.EQ.0) GOTO 9999
      ELSE
        CALL PYERRM(9,'(PYTUNE:) Tune number > max. Using defaults.')       
        GOTO 9999
      ENDIF

C...2) Hello World 
      IF (M13.GE.1) WRITE(M11,5000) CHVERS, CHDOC

C...3) Tune parameters

C=============================================================================
C...Tunes S0, S1, S2, S0A, NOCR, and RAP (by P. Skands)
      IF (ITUNE.GE.300.AND.ITUNE.LE.305) THEN 
        IF (M13.GE.1) WRITE(M11,5010) ITUNE, CHNAME
        IF (MSTP(181).LE.5.OR.(MSTP(181).EQ.6.AND.MSTP(182).LE.405))THEN
          CALL PYERRM(9,'(PYTUNE:) linked PYTHIA version incompatible'//
     &        ' with tune.')       
        ENDIF

C...PDFs
        MSTP(52)=1
        MSTP(51)=7
C...ISR
        PARP(64)=1D0
C...UE on, new model.
        MSTP(81)=21 
C...Slow IR cutoff energy scaling by default
        PARP(89)=1800D0
        PARP(90)=0.16D0
C...Switch off trial joinings
        MSTP(96)=0
C...Primordial kT cutoff
        PARP(93)=5D0

C...S0 (300), S0A (303)
        IF (ITUNE.EQ.300.OR.ITUNE.EQ.303) THEN
          IF (M13.GE.1) THEN
            CH60='see PYTHIA 6.402+ update notes,'
            WRITE(M11,5030) CH60
            CH60='M. Sandhoff & P. Skands, in hep-ph/0604120,'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & P. Skands, EPJC39(2005)129'
            WRITE(M11,5030) CH60
          ENDIF
C...Smooth ISR, low FSR
          MSTP(70)=2
          MSTP(72)=0
C...pT0
          PARP(82)=1.85D0     
C...Transverse density profile.
          MSTP(82)=5
          PARP(83)=1.6D0
C...Colour Reconnections
          MSTP(95)=6
          PARP(78)=0.20D0
          PARP(77)=0.0D0
C...  Reference energy for pT0 and energy scaling pace.
          IF (ITUNE.EQ.303) PARP(90)=0.25D0
C...Lambda_FSR scale.
          PARJ(81)=0.14D0
C...FSR activity.
          PARP(71)=4D0 
C...Rap order, Valence qq, qq x enhc, BR-g-BR supp
          MSTP(89)=1
          MSTP(88)=0
          PARP(79)=2D0         
          PARP(80)=0.01D0

C...  S1 (301)
        ELSEIF(ITUNE.EQ.301) THEN  
          IF (M13.GE.1) THEN
            CH60='see M. Sandhoff & P. Skands, in hep-ph/0604120'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & P. Skands, EPJC39(2005)129'
            WRITE(M11,5030) CH60
          ENDIF
C...  Sharp ISR, high FSR
          MSTP(70)=0
          MSTP(72)=1 
C...  pT0 
          PARP(82)=2.1D0
C...  Colour Reconnections
          MSTP(95)=2
          PARP(78)=0.35D0
C...  Transverse density profile.
          MSTP(82)=5
          PARP(83)=1.4D0
C...  Lambda_FSR scale.
          PARJ(81)=0.14D0
C...  FSR activity.
          PARP(71)=4D0 
C...  Rap order, Valence qq, qq x enhc, BR-g-BR supp
          MSTP(89)=1
          MSTP(88)=0
          PARP(79)=2D0           
          PARP(80)=0.01D0

C...  S2 (302)
        ELSEIF(ITUNE.EQ.302) THEN  
          IF (M13.GE.1) THEN
            CH60='see M. Sandhoff & P. Skands, in hep-ph/0604120'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & P. Skands, EPJC39(2005)129'
            WRITE(M11,5030) CH60
          ENDIF
C...  Smooth ISR, low FSR
          MSTP(70)=2
          MSTP(72)=0
C...  pT0
          PARP(82)=1.9D0 
C...  Transverse density profile.
          MSTP(82)=5
          PARP(83)=1.2D0
C...  Colour Reconnections
          MSTP(95)=4
          PARP(78)=0.15D0
C...  Lambda_FSR scale.
          PARJ(81)=0.14D0
C...  FSR activity.
          PARP(71)=4D0 
C...  Rap order, Valence qq, qq x enhc, BR-g-BR supp
          MSTP(89)=1
          MSTP(88)=0
          PARP(79)=2D0          
          PARP(80)=0.01D0
          
C...  NOCR (304)
        ELSEIF(ITUNE.EQ.304) THEN  
          IF (M13.GE.1) THEN
            CH60='"best try" without colour reconnections'
            WRITE(M11,5030) CH60
            CH60='see T. Sjostrand & P. Skands, EPJC39(2005)129'
            WRITE(M11,5030) CH60
          ENDIF
C...  Smooth ISR, low FSR
          MSTP(70)=2
          MSTP(72)=0
C...  pT0
          PARP(82)=2.05D0 
C...  Transverse density profile.
          MSTP(82)=5
          PARP(83)=1.8D0
C...  Colour Reconnections
          MSTP(95)=0       
C...  Lambda_FSR scale.
          PARJ(81)=0.14D0
C...  FSR activity.
          PARP(71)=4D0 
C...  Lambda order, Valence qq, large qq x enhc, BR-g-BR supp
          MSTP(89)=2
          MSTP(88)=0
          PARP(79)=3D0
          PARP(80)=0.01D0

C..."Lo FSR" retune (305)
        ELSEIF(ITUNE.EQ.305) THEN  
          IF (M13.GE.1) THEN
            CH60='"Lo FSR retune" with primitive colour reconnections'
            WRITE(M11,5030) CH60
            CH60='see T. Sjostrand & P. Skands, EPJC39(2005)129'
            WRITE(M11,5030) CH60
          ENDIF
C...  Smooth ISR, low FSR
          MSTP(70)=2
          MSTP(72)=0
C...  pT0
          PARP(82)=1.9D0         
C...  Transverse density profile.
          MSTP(82)=5
          PARP(83)=2.0D0
C...  Colour Reconnections
          MSTP(95)=1
          PARP(78)=1.0D0
C...  Lambda_FSR scale.
          PARJ(81)=0.14D0
C...  FSR activity.
          PARP(71)=4D0 
C...  Rap order, Valence qq, qq x enhc, BR-g-BR supp
          MSTP(89)=1
          MSTP(88)=0
          PARP(79)=2D0          
          PARP(80)=0.01D0          
        ENDIF
C...  Output
        IF (M13.GE.1) THEN 
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5040) 70, MSTP(70), CHMSTP(70)
          WRITE(M11,5040) 72, MSTP(72), CHMSTP(72)
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5040) 88, MSTP(88), CHMSTP(88)
          WRITE(M11,5040) 89, MSTP(89), CHMSTP(89)
          WRITE(M11,5050) 79, PARP(79), CHPARP(79)
          WRITE(M11,5050) 80, PARP(80), CHPARP(80)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)          
          WRITE(M11,5040) 95, MSTP(95), CHMSTP(95)
          WRITE(M11,5050) 78, PARP(78), CHPARP(78)
        ENDIF

C=============================================================================
C...Tunes A, AW, BW, DW, DWT, and QW (by R.D. Field, CDF) (100-105)
C...and ATLAS Tune (by A. Moraes, ATLAS) (106)
      ELSEIF (ITUNE.GE.100.AND.ITUNE.LE.106) THEN
        IF (M13.GE.1.AND.ITUNE.NE.106) THEN 
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='see R.D. Field (CDF), in hep-ph/0610012'
          WRITE(M11,5030) CH60 
          CH60='and T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
          WRITE(M11,5030) CH60
        ENDIF
C...Multiple interactions on, old framework
        MSTP(81)=1
C...Fast IR cutoff energy scaling by default
        PARP(89)=1800D0
        PARP(90)=0.25D0
C...Default CTEQ5L (internal), except for QW: CTEQ61 (external)
        MSTP(51)=7
        MSTP(52)=1
        IF (ITUNE.EQ.105) THEN 
          MSTP(51)=10150
          MSTP(52)=2
        ENDIF
C...Double Gaussian matter distribution. 
        MSTP(82)=4
        PARP(83)=0.5D0
        PARP(84)=0.4D0
C...FSR activity. 
        PARP(71)=4D0
C...Lambda_FSR scale. 
        PARJ(81)=0.29D0     

C...Tune A and AW 
        IF(ITUNE.EQ.100.OR.ITUNE.EQ.101) THEN
C...pT0.
          PARP(82)=2.0D0
c...String drawing almost completely minimizes string length.
          PARP(85)=0.9D0
          PARP(86)=0.95D0
C...ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1D0
          PARP(64)=1D0
          PARP(67)=4D0
C...Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=1D0
          PARP(93)=5D0
C...AW : higher ISR IR cutoff, but also larger alpha_s and more intrinsic kT.
          IF (ITUNE.EQ.101) THEN
            PARP(62)=1.25D0
            PARP(64)=0.2D0
            PARP(91)=2.1D0
            PARP(92)=15.0D0
          ENDIF
          
C...  Tune BW (larger alpha_s, more intrinsic kT. Smaller ISR phase space.)
        ELSEIF (ITUNE.EQ.102) THEN
C...  pT0.
          PARP(82)=1.9D0
c...  String drawing completely minimizes string length.
          PARP(85)=1.0D0
          PARP(86)=1.0D0
C...  ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1.25D0
          PARP(64)=0.2D0
          PARP(67)=1D0
C...  Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=2.1D0
          PARP(93)=15D0

C...  Tune DW
        ELSEIF (ITUNE.EQ.103) THEN
C...  pT0.
          PARP(82)=1.9D0
c...  String drawing completely minimizes string length.
          PARP(85)=1.0D0
          PARP(86)=1.0D0
C...  ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1.25D0
          PARP(64)=0.2D0
          PARP(67)=2.5D0
C...  Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=2.1D0
          PARP(93)=15D0

C...  Tune DWT
        ELSEIF (ITUNE.EQ.104) THEN
C...  pT0.
          PARP(82)=1.9409D0
C... Run II ref scale and slow scaling
          PARP(89)=1960D0
          PARP(90)=0.16D0
c...  String drawing completely minimizes string length.
          PARP(85)=1.0D0
          PARP(86)=1.0D0
C...  ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1.25D0
          PARP(64)=0.2D0
          PARP(67)=2.5D0
C...  Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=2.1D0
          PARP(93)=15D0

C...Tune QW
        ELSEIF(ITUNE.EQ.105) THEN
          IF (M13.GE.1) THEN 
            WRITE(M11,5030) ' '
            CH70='NB! This tune requires CTEQ6.1 pdfs to be '//
     &           'externally linked and'
            WRITE(M11,5035) CH70
            CH70='MSTP(51) should be set manually according to '//
     &          'the library used'
            WRITE(M11,5035) CH70
          ENDIF
C...  pT0.
          PARP(82)=1.1D0
c...  String drawing completely minimizes string length.
          PARP(85)=1.0D0
          PARP(86)=1.0D0
C...  ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1.25D0
          PARP(64)=0.2D0
          PARP(67)=2.5D0
C...  Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=2.1D0
          PARP(93)=15D0

C...ATLAS Tune
        ELSEIF(ITUNE.EQ.106) THEN
          IF (M13.GE.1) THEN 
            WRITE(M11,5010) ITUNE, CHNAME
            CH60='see A. Moraes et al., SN-ATLAS-2006-057'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
            WRITE(M11,5030) CH60
          ENDIF
C...  pT0.
          PARP(82)=1.8D0
C...  Different ref and rescaling pacee
          PARP(89)=1000D0
          PARP(90)=0.16D0
C...  Parameters of mass distribution
          PARP(83)=0.5D0
          PARP(84)=0.5D0
C...  Old default string drawing
          PARP(85)=0.33D0
          PARP(86)=0.66D0
C...  ISR, phase space equivalent to Tune B
          PARP(62)=1D0
          PARP(64)=1D0
          PARP(67)=1D0
C...  FSR
          PARP(71)=4D0
          PARJ(81)=0.29D0
C...  Intrinsic kT
          MSTP(91)=1
          PARP(91)=1D0
          PARP(93)=5D0
        ENDIF
        
C...  Output
        IF (M13.GE.1) THEN 
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5050) 67, PARP(67), CHPARP(67)
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5050) 84, PARP(84), CHPARP(84)
          WRITE(M11,5050) 85, PARP(85), CHPARP(85)
          WRITE(M11,5050) 86, PARP(86), CHPARP(86)
          WRITE(M11,5040) 91, MSTP(91), CHMSTP(91)
          WRITE(M11,5050) 91, PARP(91), CHPARP(91)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)          
        ENDIF     

C=============================================================================
C... ACR, tune A with new CR (107)
      ELSEIF(ITUNE.EQ.107) THEN
        IF (M13.GE.1) THEN 
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='Tune A modified with new colour reconnections'
          WRITE(M11,5030) CH60
          CH60='PARP(85)=0D0 and amount of CR is regulated by PARP(78)'
          WRITE(M11,5030) CH60 
        ENDIF
        IF (MSTP(181).LE.5.OR.(MSTP(181).EQ.6.AND.MSTP(182).LE.406))THEN
          CALL PYERRM(9,'(PYTUNE:) linked PYTHIA version incompatible'//
     &        ' with tune. Using defaults.')       
          GOTO 9998
        ENDIF
        MSTP(81)=1
        PARP(89)=1800D0
        PARP(90)=0.25D0
        MSTP(82)=4
        PARP(83)=0.5D0
        PARP(84)=0.4D0
        MSTP(51)=7
        MSTP(52)=1
        PARP(71)=4D0
        PARJ(81)=0.29D0
        PARP(82)=2.0D0
        PARP(85)=0.0D0
        PARP(86)=0.66D0
        PARP(62)=1D0
        PARP(64)=1D0
        PARP(67)=4D0
        MSTP(91)=1
        PARP(91)=1D0
        PARP(93)=5D0
        MSTP(95)=6
        PARP(78)=0.25D0
C...Output
        IF (M13.GE.1) THEN 
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5050) 67, PARP(67), CHPARP(67)
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5050) 84, PARP(84), CHPARP(84)
          WRITE(M11,5050) 85, PARP(85), CHPARP(85)
          WRITE(M11,5050) 86, PARP(86), CHPARP(86)
          WRITE(M11,5040) 91, MSTP(91), CHMSTP(91)
          WRITE(M11,5050) 91, PARP(91), CHPARP(91)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)          
          WRITE(M11,5040) 95, MSTP(95), CHMSTP(95)
          WRITE(M11,5050) 78, PARP(78), CHPARP(78)
        ENDIF

C=============================================================================
C...  Intermediate model. Rap tune (retuned to post-6.406 IR factorization)
      ELSEIF(ITUNE.EQ.200) THEN
        IF (M13.GE.1) THEN 
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='see T. Sjostrand & P. Skands, JHEP03(2004)053'
          WRITE(M11,5030) CH60
        ENDIF
        IF (MSTP(181).LE.5.OR.(MSTP(181).EQ.6.AND.MSTP(182).LE.405))THEN
          CALL PYERRM(9,'(PYTUNE:) linked PYTHIA version incompatible'//
     &        ' with tune.')       
        ENDIF
C...PDF
        MSTP(51)=7
        MSTP(52)=1
C...ISR 
        PARP(62)=1D0
        PARP(64)=1D0
        PARP(67)=4D0
C...FSR
        PARP(71)=4D0
        PARJ(81)=0.29D0
C...UE
        MSTP(81)=11
        PARP(82)=2.25D0
        PARP(89)=1800D0
        PARP(90)=0.25D0
C...  ExpOfPow(1.8) overlap profile
        MSTP(82)=5
        PARP(83)=1.8D0
C...  Valence qq
        MSTP(88)=0
C...  Rap Tune
        MSTP(89)=1
C...  Default diquark, BR-g-BR supp
        PARP(79)=2D0           
        PARP(80)=0.01D0
C...  Final state reconnect.
        MSTP(95)=1
        PARP(78)=0.55D0 
C...  Output
        IF (M13.GE.1) THEN 
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5050) 67, PARP(67), CHPARP(67)
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5040) 88, MSTP(88), CHMSTP(88)
          WRITE(M11,5040) 89, MSTP(89), CHMSTP(89)
          WRITE(M11,5050) 79, PARP(79), CHPARP(79)
          WRITE(M11,5050) 80, PARP(80), CHPARP(80)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)          
          WRITE(M11,5040) 95, MSTP(95), CHMSTP(95)
          WRITE(M11,5050) 78, PARP(78), CHPARP(78)
        ENDIF

C=============================================================================
C...Uppsala models: Generalized Area Law and Soft Colour Interactions
      ELSEIF(CHNAME.EQ.'GAL Tune 0'.OR.CHNAME.EQ.'GAL Tune 1') THEN
        IF (M13.GE.1) THEN 
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='see J. Rathsman, PLB452(1999)364'
          WRITE(M11,5030) CH60
C ?         CH60='A. Edin, G. Ingelman, J. Rathsman, hep-ph/9912539,'
C ?         WRITE(M11,5030)
          CH60='and T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
          WRITE(M11,5030) CH60          
          WRITE(M11,5030) ' '    
          CH70='NB! The GAL model must be run with modified '//
     &        'Pythia v6.215:'
          WRITE(M11,5035) CH70
          CH70='available from http://www.isv.uu.se/thep/MC/scigal/'
          WRITE(M11,5035) CH70
          WRITE(M11,5030) ' '
        ENDIF
C...GAL Recommended settings from Uppsala web page (as per 22/08 2006)
        MSWI(2) = 3
        PARSCI(2) = 0.10
        MSWI(1) = 2
        PARSCI(1) = 0.44
        MSTJ(16) = 0
        PARJ(42) = 0.45
        PARJ(82) = 2.0
        PARP(62) = 2.0	
        MSTP(81) = 1
        MSTP(82) = 1
        PARP(81) = 1.9
        MSTP(92) = 1
        IF(CHNAME.EQ.'GAL Tune 1') THEN
C...GAL retune (P. Skands) to get better min-bias <Nch> at Tevatron
          MSTP(82)=4
          PARP(83)=0.25D0
          PARP(84)=0.5D0
          PARP(82) = 1.75
          IF (M13.GE.1) THEN 
            WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
            WRITE(M11,5050) 82, PARP(82), CHPARP(82)
            WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
            WRITE(M11,5050) 83, PARP(83), CHPARP(83)
            WRITE(M11,5050) 84, PARP(84), CHPARP(84)
          ENDIF
        ELSE
          IF (M13.GE.1) THEN
            WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
            WRITE(M11,5050) 81, PARP(81), CHPARP(81)
            WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          ENDIF
        ENDIF
C...Output
        IF (M13.GE.1) THEN
          WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          WRITE(M11,5060) 82, PARJ(82), CHPARJ(82)
          WRITE(M11,5040) 92, MSTP(92), CHMSTP(92)
          CH40='FSI SCI/GAL selection'
          WRITE(M11,6040) 1, MSWI(1), CH40
          CH40='FSI SCI/GAL sea quark treatment'
          WRITE(M11,6040) 2, MSWI(2), CH40
          CH40='FSI SCI/GAL sea quark treatment parm'
          WRITE(M11,6050) 1, PARSCI(1), CH40
          CH40='FSI SCI/GAL string reco probability R_0'
          WRITE(M11,6050) 2, PARSCI(2), CH40 
          WRITE(M11,5060) 42, PARJ(42), CHPARJ(42)
          WRITE(M11,5070) 16, MSTJ(16), CHMSTJ(16)
        ENDIF
      ELSEIF(CHNAME.EQ.'SCI Tune 0'.OR.CHNAME.EQ.'SCI Tune 1') THEN
        IF (M13.GE.1) THEN 
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='see A.Edin et al, PLB366(1996)371, Z.Phys.C75(1997)57,'
          WRITE(M11,5030) CH60
          CH60='and T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
          WRITE(M11,5030) CH60          
          WRITE(M11,5030) ' '    
          CH70='NB! The SCI model must be run with modified '//
     &        'Pythia v6.215:'
          WRITE(M11,5035) CH70
          CH70='available from http://www.isv.uu.se/thep/MC/scigal/'
          WRITE(M11,5035) CH70
          WRITE(M11,5030) ' '
        ENDIF
C...SCI Recommended settings from Uppsala web page (as per 22/08 2006)
        MSTP(81)=1
        MSTP(82)=1
        PARP(81)=2.2
        MSTP(92)=1        
        MSWI(2)=2               
        PARSCI(2)=0.50          
        MSWI(1)=2               
        PARSCI(1)=0.44          
        MSTJ(16)=0              
        IF (CHNAME.EQ.'SCI Tune 1') THEN
C...SCI retune (P. Skands) to get better min-bias <Nch> at Tevatron
          MSTP(81) = 1
          MSTP(82) = 3
          PARP(82) = 2.4
          PARP(83) = 0.5D0
          PARP(62) = 1.5
          PARP(84)=0.25D0        
          IF (M13.GE.1) THEN 
            WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
            WRITE(M11,5050) 82, PARP(82), CHPARP(82)
            WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
            WRITE(M11,5050) 83, PARP(83), CHPARP(83)
            WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          ENDIF
        ELSE
          IF (M13.GE.1) THEN
            WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
            WRITE(M11,5050) 81, PARP(81), CHPARP(81)
            WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          ENDIF
        ENDIF
C...Output
        IF (M13.GE.1) THEN 
          WRITE(M11,5040) 92, MSTP(92), CHMSTP(92)
          CH40='FSI SCI/GAL selection'
          WRITE(M11,6040) 1, MSWI(1), CH40
          CH40='FSI SCI/GAL sea quark treatment'
          WRITE(M11,6040) 2, MSWI(2), CH40
          CH40='FSI SCI/GAL sea quark treatment parm'
          WRITE(M11,6050) 1, PARSCI(1), CH40
          CH40='FSI SCI/GAL string reco probability R_0'
          WRITE(M11,6050) 2, PARSCI(2), CH40 
          WRITE(M11,5070) 16, MSTJ(16), CHMSTJ(16)
        ENDIF

      ELSE
        IF (MSTU(13).GE.1) WRITE(M11,5020) ITUNE

      ENDIF   
 
 9998 IF (MSTU(13).GE.1) WRITE(M11,6000) 

 9999 RETURN 

 5000 FORMAT(1x,78('*')/' *',76x,'*'/' *',3x,'PYTUNE v',A6,' : ',
     &    'Presets for underlying-event (and min-bias)',13x,'*'/' *',
     &    20x,'Last Change : ',A8,' - P. Skands',22x,'*'/' *',76x,'*')
 5010 FORMAT(' *',3x,I4,1x,A16,52x,'*')
 5020 FORMAT(' *',3x,'Tune ',I4, ' not recognized. Using defaults.')
 5030 FORMAT(' *',3x,10x,A60,3x,'*')
 5035 FORMAT(' *',3x,A70,3x,'*')
 5040 FORMAT(' *',5x,'MSTP(',I2,') = ',I12,3x,A40,5x,'*')
 5050 FORMAT(' *',5x,'PARP(',I2,') = ',F12.4,3x,A40,5x,'*')
 5060 FORMAT(' *',5x,'PARJ(',I2,') = ',F12.4,3x,A40,5x,'*')
 5070 FORMAT(' *',5x,'MSTJ(',I2,') = ',I12,3x,A40,5x,'*')
 5140 FORMAT(' *',5x,'MSTP(',I3,')= ',I12,3x,A40,5x,'*')
 5150 FORMAT(' *',5x,'PARP(',I3,')= ',F12.4,3x,A40,5x,'*')
 6000 FORMAT(' *',76x,'*'/1x,32('*'),1x,'END OF PYTUNE',1x,31('*')) 
 6040 FORMAT(' *',5x,'MSWI(',I1,')  = ',I12,3x,A40,5x,'*')
 6050 FORMAT(' *',5x,'PARSCI(',I1,')= ',F12.4,3x,A40,5x,'*')

      END 
