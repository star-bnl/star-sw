cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYEVOL
C...Handles intertwined pT-ordered spacelike initial-state parton
C...and multiple interactions.
 
      SUBROUTINE PYEVOL(MODE,PT2MAX,PT2MIN)
C...Mode = -1 : Initialize first time. Determine MAX and MIN scales.
C...MODE =  0 : (Re-)initialize ISR/MI evolution.
C...Mode =  1 : Evolve event from PT2MAX to PT2MIN.
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...External
      EXTERNAL PYALPS
      DOUBLE PRECISION PYALPS
C...Parameter statement for maximum size of showers.
      PARAMETER (MAXNUR=1000)
C...Commonblocks.
      include 'inc/pypart'
      include 'inc/pyjets'
      include 'inc/pydat1'
      include 'inc/pydat2'
      include 'inc/pypars'
      include 'inc/pyint1'
      include 'inc/pyint2'
      include 'inc/pyint3'
      include 'inc/pyintm'
      include 'inc/pyctag'
      include 'inc/pyismx'
      include 'inc/pyisjn'
C...Max size of hard system = HEPEUP size
      INTEGER MAXNUP
      PARAMETER (MAXNUP=500)
C...Local arrays and saved variables.
      DIMENSION VINTSV(11:80),KSAV(MAXNUP,5),PSAV(MAXNUP,5),
     &     VSAV(MAXNUP,5),SHAT(240)
      SAVE NSAV,NPARTS,M15SV,M16SV,M21SV,M22SV,VINTSV,SHAT,ISUBHD,ALAM3
     &     ,PSAV,KSAV,VSAV
 
 
C----------------------------------------------------------------------
C...MODE=-1: Pre-initialization. Store info on hard scattering etc,
C...done only once per event, while MODE=0 is repeated each time the
C...evolution needs to be restarted.
      IF (MODE.EQ.-1) THEN
        ISUBHD=MINT(1)
        NSAV=N
        NPARTS=NPART
C...Store hard scattering variables
        M15SV=MINT(15)
        M16SV=MINT(16)
        M21SV=MINT(21)
        M22SV=MINT(22)
        DO 100 J=11,80
          VINTSV(J)=VINT(J)
  100   CONTINUE
        DO 120 J=1,5
          DO 110 IS=1,NSAV-MINT(84)
            I=IS+MINT(84)
            PSAV(IS,J)=P(I,J)
            KSAV(IS,J)=K(I,J)
            VSAV(IS,J)=V(I,J)
  110     CONTINUE
  120   CONTINUE
 
C...Set shat for hardest scattering
        SHAT(1)=VINT(44)
        IF(ISET(ISUBHD).GE.3.AND.ISET(ISUBHD).LE.5) SHAT(1)=VINT(26)
     &       *VINT(2)
 
C...Compute 3-Flavour Lambda_QCD (sets absolute lowest PT scale below)
        RMC=PMAS(4,1)
        RMB=PMAS(5,1)
        ALAM4=PARP(61)
        IF(MSTU(112).LT.4) ALAM4=PARP(61)*(PARP(61)/RMC)**(2D0/25D0)
        IF(MSTU(112).GT.4) ALAM4=PARP(61)*(RMB/PARP(61))**(2D0/25D0)
        ALAM3=ALAM4*(RMC/ALAM4)**(2D0/27D0)
 
C----------------------------------------------------------------------
C...MODE= 0: Initialize ISR/MI evolution, i.e. begin from hardest
C...interaction initiators, with no previous evolution. Check the input
C...PT2MAX and PT2MIN and impose extra constraints on minimum PT2 (e.g.
C...must be larger than Lambda_QCD) and maximum PT2 (e.g. must be
C...smaller than the CM energy / 2.)
      ELSEIF (MODE.EQ.0) THEN
C...Reset counters and switches
        N=NSAV
        NPART=NPARTS
        MINT(30)=0
        MINT(31)=1
        MINT(36)=1
C...Reset hard scattering variables
        MINT(1)=ISUBHD
        DO 130 J=11,80
          VINT(J)=VINTSV(J)
  130   CONTINUE
        DO 150 J=1,5
          DO 140 IS=1,NSAV-MINT(84)
            I=IS+MINT(84)
            P(I,J)=PSAV(IS,J)
            K(I,J)=KSAV(IS,J)
            V(I,J)=VSAV(IS,J)
            P(MINT(83)+4+IS,J)=PSAV(IS,J)
            V(MINT(83)+4+IS,J)=VSAV(IS,J)
  140     CONTINUE
  150   CONTINUE
C...Reset statistics on activity in event.
        DO 160 J=351,359
          MINT(J)=0
          VINT(J)=0D0
  160   CONTINUE
C...Reset extra companion reweighting factor
        VINT(140)=1D0
 
C...We do not generate MI for soft process (ISUB=95), but the
C...initialization must be done regardless, for later purposes.
        MINT(36)=1
 
C...Initialize multiple interactions.
        CALL PYPTMI(-1,PTDUM1,PTDUM2,PTDUM3,IDUM)
        IF(MINT(51).NE.0) RETURN
 
C...Decide whether quarks in hard scattering were valence or sea
        PT2HD=VINT(54)
        DO 170 JS=1,2
          MINT(30)=JS
          CALL PYPTMI(2,PT2HD,PTDUM2,PTDUM3,IDUM)
          IF(MINT(51).NE.0) RETURN
  170   CONTINUE
 
C...Set lower cutoff for PT2 iteration and colour interference PT2 scale
        VINT(18)=0D0
        PT2MIN=MAX(PT2MIN,(1.1D0*ALAM3)**2)
        IF (MSTP(70).EQ.2) THEN
C...VINT(18) is freezeout scale of alpha_s: alpha_eff(0) = alpha_s(VINT(18))
          VINT(18)=(PARP(82)*(VINT(1)/PARP(89))**PARP(90))**2
        ELSEIF (MSTP(70).EQ.3) THEN
C...MSTP(70) = 3 : Derive VINT(18) from alpha_eff(Lambda3) = PARP(73) 
          ALPHA0 = MAX(1D-6,PARP(73))
          Q20 = ALAM3**2/PARP(64)
          IF (MSTP(64).EQ.3) Q20 = Q20 * 1.661**2
          VINT(18) = Q20 * (EXP(12*PARU(1)/27D0/ALPHA0)-1D0)
        ENDIF
C...Also store PT2MIN in VINT(17).
        VINT(17)=PT2MIN
 
C...Set FS masses zero now.
        VINT(63)=0D0
        VINT(64)=0D0
 
C...Initialize IS showers with VINT(56) as max scale.
        PT2ISR=VINT(56)
        PT20=PT2MIN
        IF (MSTP(70).EQ.0) THEN 
          PT20=MAX(PT2MIN,PARP(62)**2)
        ELSEIF (MSTP(70).EQ.1) THEN
          PT20=MAX(PT2MIN,(PARP(81)*(VINT(1)/PARP(89))**PARP(90))**2)
        ENDIF  
        CALL PYPTIS(-1,PT2ISR,PT20,PT2DUM,IFAIL)
        IF(MINT(51).NE.0) RETURN
 
        RETURN
 
C----------------------------------------------------------------------
C...MODE= 1: Evolve event from PTMAX to PTMIN.
      ELSEIF (MODE.EQ.1) THEN
 
C...Skip if no phase space.
        IF (PT2MAX.LE.PT2MIN) GOTO 330
 
C...Starting pT2 max scale (to be udpated successively).
        PT2CMX=PT2MAX
 
C...Evolve two sides of the event to find which branches at highest pT.
  200   JSMX=-1
        MIMX=0
        PT2MX=0D0
 
C...Loop over current shower initiators.
        IF (MSTP(61).GE.1) THEN
          DO 230 MI=1,MINT(31)
            IF (MI.GE.2.AND.MSTP(84).LE.0) GOTO 230
            ISUB=96
            IF (MI.EQ.1) ISUB=ISUBHD
            MINT(1)=ISUB
            MINT(36)=MI
C...Set up shat, initiator x values, and x remaining in BR.
            VINT(44)=SHAT(MI)
            VINT(141)=XMI(1,MI)
            VINT(142)=XMI(2,MI)
            VINT(143)=1D0
            VINT(144)=1D0
            DO 210 JI=1,MINT(31)
              IF (JI.EQ.MINT(36)) GOTO 210
              VINT(143)=VINT(143)-XMI(1,JI)
              VINT(144)=VINT(144)-XMI(2,JI)
  210       CONTINUE
C...Loop over sides.
C...Generate trial branchings for this interaction. The hardest
C...branching so far is automatically updated if necessary in /PYISMX/.
            DO 220 JS=1,2
              MINT(30)=JS
              PT20=PT2MIN
              IF (MSTP(70).EQ.0) THEN 
                PT20=MAX(PT2MIN,PARP(62)**2)
              ELSEIF (MSTP(70).EQ.1) THEN
                PT20=MAX(PT2MIN,
     &              (PARP(81)*(VINT(1)/PARP(89))**PARP(90))**2)
              ENDIF  
              CALL PYPTIS(0,PT2CMX,PT20,PT2NEW,IFAIL)
              IF (MINT(51).NE.0) RETURN
  220       CONTINUE
  230     CONTINUE
        ENDIF
 
C...Generate trial additional interaction.
        MINT(36)=MINT(31)+1
        IF (MOD(MSTP(81),10).GE.1) THEN
          MINT(1)=96
C...Set up X remaining in BR.
          VINT(143)=1D0
          VINT(144)=1D0
          DO 250 JI=1,MINT(31)
            VINT(143)=VINT(143)-XMI(1,JI)
            VINT(144)=VINT(144)-XMI(2,JI)
  250     CONTINUE
C...Generate trial interaction
          CALL PYPTMI(0,PT2CMX,PT2MIN,PT2NEW,IFAIL)
          IF (MINT(51).EQ.1) RETURN
        ENDIF
 
C...And the winner is:
        IF (PT2MX.LT.PT2MIN) THEN
          GOTO 330
        ELSEIF (JSMX.EQ.0) THEN
C...Accept additional interaction (may still fail).
          CALL PYPTMI(1,PT2NEW,PT2MIN,PT2DUM,IFAIL)
          IF(MINT(51).NE.0) RETURN
          IF (IFAIL.EQ.0) THEN
            SHAT(MINT(36))=VINT(44)
C...Decide on flavours (valence/sea/companion).
            DO 270 JS=1,2
              MINT(30)=JS
              CALL PYPTMI(2,PT2NEW,PT2MIN,PT2DUM,IFAIL)
              IF(MINT(51).NE.0) RETURN
  270       CONTINUE
          ENDIF
        ELSEIF (JSMX.EQ.1.OR.JSMX.EQ.2) THEN
C...Reconstruct kinematics of acceptable ISR branching.
C...Set up shat, initiator x values, and x remaining in BR.
          MINT(30)=JSMX
          MINT(36)=MIMX
          VINT(44)=SHAT(MINT(36))
          VINT(141)=XMI(1,MINT(36))
          VINT(142)=XMI(2,MINT(36))
          VINT(143)=1D0
          VINT(144)=1D0
          DO 280 JI=1,MINT(31)
            IF (JI.EQ.MINT(36)) GOTO 280
            VINT(143)=VINT(143)-XMI(1,JI)
            VINT(144)=VINT(144)-XMI(2,JI)
  280     CONTINUE
          PT2NEW=PT2MX
          CALL PYPTIS(1,PT2NEW,PT2DM1,PT2DM2,IFAIL)
          IF (MINT(51).EQ.1) RETURN
        ELSEIF (JSMX.EQ.3.OR.JSMX.EQ.4) THEN
C...Bookeep joining. Cannot (yet) be constructed kinematically.
          MINT(354)=MINT(354)+1
          VINT(354)=VINT(354)+SQRT(PT2MX)
          IF (MINT(354).EQ.1) VINT(359)=SQRT(PT2MX)
          MJOIND(JSMX-2,MJN1MX)=MJN2MX
          MJOIND(JSMX-2,MJN2MX)=MJN1MX
        ENDIF
 
C...Update PT2 iteration scale.
        PT2CMX=PT2MX
 
C...Loop back to continue evolution.
        IF(N.GT.MSTU(4)-MSTU(32)-10) THEN
          CALL PYERRM(11,'(PYEVOL:) no more memory left in PYJETS')
        ELSE
          IF (JSMX.GE.0.AND.PT2CMX.GE.PT2MIN) GOTO 200
        ENDIF
 
C----------------------------------------------------------------------
C...MODE= 2: (Re-)store user information on hardest interaction etc.
      ELSEIF (MODE.EQ.2) THEN
 
C...Revert to "ordinary" meanings of some parameters.
        DO 310 JS=1,2
          MINT(12+JS)=K(IMI(JS,1,1),2)
          VINT(140+JS)=XMI(JS,1)
          IF(MINT(18+JS).EQ.1) VINT(140+JS)=VINT(154+JS)*XMI(JS,1)
          VINT(142+JS)=1D0
          DO 300 MI=1,MINT(31)
            VINT(142+JS)=VINT(142+JS)-XMI(JS,MI)
  300     CONTINUE
  310   CONTINUE
 
C...Restore saved quantities for hardest interaction.
        MINT(1)=ISUBHD
        MINT(15)=M15SV
        MINT(16)=M16SV
        MINT(21)=M21SV
        MINT(22)=M22SV
        DO 320 J=11,80
          VINT(J)=VINTSV(J)
  320   CONTINUE
 
      ENDIF
 
  330 RETURN
      END
