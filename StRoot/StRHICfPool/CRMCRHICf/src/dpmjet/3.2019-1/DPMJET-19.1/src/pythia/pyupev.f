cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYUPEV
C...Administers the hard-process generation required for output to the
C...Les Houches event record.
 
      SUBROUTINE PYUPEV
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYCOMP
 
C...Commonblocks.
      include 'inc/pyjets'
      include 'inc/pyctag'
      include 'inc/pydat1'
      include 'inc/pydat2'
      include 'inc/pydat3'
      include 'inc/pypars'
      include 'inc/pyint1'
      include 'inc/pyint2'
      include 'inc/pyint4'
 
C...HEPEUP for output.
      include 'inc/hepeup'
 
C...Stop if no subprocesses on.
      IF(MINT(121).EQ.1.AND.MSTI(53).EQ.1) THEN
        WRITE(MSTU(11),5100)
        STOP
      ENDIF
 
C...Special flags for hard-process generation only.
      MSTP71=MSTP(71)
      MSTP(71)=0
      MST128=MSTP(128)
      MSTP(128)=1
 
C...Initial values for some counters.
      N=0
      MINT(5)=MINT(5)+1
      MINT(7)=0
      MINT(8)=0
      MINT(30)=0
      MINT(83)=0
      MINT(84)=MSTP(126)
      MSTU(24)=0
      MSTU70=0
C unvar       MSTJ14=MSTJ(14)
C...Normally, use K(I,4:5) colour info rather than /PYCTAG/.
      MINT(33)=0
 
C...If variable energies: redo incoming kinematics and cross-section.
      MSTI(61)=0
      IF(MSTP(171).EQ.1) THEN
        CALL PYINKI(1)
        IF(MSTI(61).EQ.1) THEN
          MINT(5)=MINT(5)-1
          RETURN
        ENDIF
        IF(MINT(121).GT.1) CALL PYSAVE(3,1)
        CALL PYXTOT
      ENDIF
 
C...Do not allow pileup events.
      MINT(82)=1
 
C...Generate variables of hard scattering.
      MINT(51)=0
      MSTI(52)=0
  100 CONTINUE
      IF(MINT(51).NE.0.OR.MSTU(24).NE.0) MSTI(52)=MSTI(52)+1
      MINT(31)=0
      MINT(51)=0
      MINT(57)=0
      CALL PYRAND
      IF(MSTI(61).EQ.1) THEN
        MINT(5)=MINT(5)-1
        RETURN
      ENDIF
      IF(MINT(51).EQ.2) RETURN
      ISUB=MINT(1)
 
      IF((ISUB.LE.90.OR.ISUB.GE.95).AND.ISUB.NE.99) THEN
C...Hard scattering (including low-pT):
C...reconstruct kinematics and colour flow of hard scattering.
        MINT31=MINT(31)
        MINT(31)=MINT31
        MINT(51)=0
        CALL PYSCAT
        IF(MINT(51).EQ.1) GOTO 100
C unvar IPU1=MINT(84)+1
C unvar IPU2=MINT(84)+2
 
C...Decay of final state resonances.
        MINT(32)=0
        IF(MSTP(41).GE.1.AND.ISET(ISUB).LE.10.AND.ISUB.NE.95)
     &  CALL PYRESD(0)
        IF(MINT(51).EQ.1) GOTO 100
        MINT(52)=N
 
C...Longitudinal boost of hard scattering.
        BETAZ=(VINT(41)-VINT(42))/(VINT(41)+VINT(42))
        CALL PYROBO(MINT(84)+1,N,0D0,0D0,0D0,0D0,BETAZ)
 
      ELSEIF(ISUB.NE.99) THEN
C...Diffractive and elastic scattering.
        CALL PYDIFF
 
      ELSE
C...DIS scattering (photon flux external).
        CALL PYDISG
        IF(MINT(51).EQ.1) GOTO 100
      ENDIF
 
C...Check that no odd resonance left undecayed.
      MINT(54)=N
      NFIX=N
      DO 120 I=MINT(84)+1,NFIX
        IF(K(I,1).GE.1.AND.K(I,1).LE.10.AND.K(I,2).NE.21.AND.
     &  K(I,2).NE.22) THEN
          KCA=PYCOMP(K(I,2))
          IF(MWID(KCA).NE.0.AND.MDCY(KCA,1).GE.1) THEN
            CALL PYRESD(I)
            IF(MINT(51).EQ.1) GOTO 100
          ENDIF
        ENDIF
  120 CONTINUE
 
C...Boost hadronic subsystem to overall rest frame.
C..(Only relevant when photon inside lepton beam.)
      IF(MINT(141).NE.0.OR.MINT(142).NE.0) CALL PYGAGA(4,WTGAGA)
 
C...Store event information and calculate Monte Carlo estimates of
C...subprocess cross-sections.
      CALL PYDOCU
 
C...Transform to the desired coordinate frame.
      CALL PYFRAM(MSTP(124))
      MSTU(70)=MSTU70
      PARU(21)=VINT(1)
 
C...Restore special flags for hard-process generation only.
      MSTP(71)=MSTP71
      MSTP(128)=MST128
 
C...Trace colour tags; convert to LHA style labels.
      NCT=100
      DO 150 I=MINT(84)+1,N
        MCT(I,1)=0
        MCT(I,2)=0
  150 CONTINUE
      DO 160 I=MINT(84)+1,N
        KQ=KCHG(PYCOMP(K(I,2)),2)*SIGN(1,K(I,2))
        IF(K(I,1).EQ.3.OR.K(I,1).EQ.13.OR.K(I,1).EQ.14) THEN
          IF(K(I,4).NE.0.AND.(KQ.EQ.1.OR.KQ.EQ.2).AND.MCT(I,1).EQ.0)
     &    THEN
            IMO=MOD(K(I,4)/MSTU(5),MSTU(5))
            IDA=MOD(K(I,4),MSTU(5))
            IF(IMO.NE.0.AND.MOD(K(IMO,5)/MSTU(5),MSTU(5)).EQ.I.AND.
     &      MCT(IMO,2).NE.0) THEN
              MCT(I,1)=MCT(IMO,2)
            ELSEIF(IMO.NE.0.AND.MOD(K(IMO,4),MSTU(5)).EQ.I.AND.
     &      MCT(IMO,1).NE.0) THEN
              MCT(I,1)=MCT(IMO,1)
            ELSEIF(IDA.NE.0.AND.MOD(K(IDA,5),MSTU(5)).EQ.I.AND.
     &      MCT(IDA,2).NE.0) THEN
              MCT(I,1)=MCT(IDA,2)
            ELSE
              NCT=NCT+1
              MCT(I,1)=NCT
            ENDIF
          ENDIF
          IF(K(I,5).NE.0.AND.(KQ.EQ.-1.OR.KQ.EQ.2).AND.MCT(I,2).EQ.0)
     &    THEN
            IMO=MOD(K(I,5)/MSTU(5),MSTU(5))
            IDA=MOD(K(I,5),MSTU(5))
            IF(IMO.NE.0.AND.MOD(K(IMO,4)/MSTU(5),MSTU(5)).EQ.I.AND.
     &      MCT(IMO,1).NE.0) THEN
              MCT(I,2)=MCT(IMO,1)
            ELSEIF(IMO.NE.0.AND.MOD(K(IMO,5),MSTU(5)).EQ.I.AND.
     &      MCT(IMO,2).NE.0) THEN
              MCT(I,2)=MCT(IMO,2)
            ELSEIF(IDA.NE.0.AND.MOD(K(IDA,4),MSTU(5)).EQ.I.AND.
     &      MCT(IDA,1).NE.0) THEN
              MCT(I,2)=MCT(IDA,1)
            ELSE
              NCT=NCT+1
              MCT(I,2)=NCT
            ENDIF
          ENDIF
        ENDIF
  160 CONTINUE
 
C...Put event in HEPEUP commonblock.
      NUP=N-MINT(84)
      IDPRUP=MINT(1)
      XWGTUP=1D0
      SCALUP=VINT(53)
      AQEDUP=VINT(57)
      AQCDUP=VINT(58)
      DO 180 I=1,NUP
        IDUP(I)=K(I+MINT(84),2)
        IF(I.LE.2) THEN
          ISTUP(I)=-1
          MOTHUP(1,I)=0
          MOTHUP(2,I)=0
        ELSEIF(K(I+4,3).EQ.0) THEN
          ISTUP(I)=1
          MOTHUP(1,I)=1
          MOTHUP(2,I)=2
        ELSE
          ISTUP(I)=1
          MOTHUP(1,I)=K(I+MINT(84),3)-MINT(84)
          MOTHUP(2,I)=0
        ENDIF
        IF(I.GE.3.AND.K(I+MINT(84),3).GT.0)
     &  ISTUP(K(I+MINT(84),3)-MINT(84))=2
        ICOLUP(1,I)=MCT(I+MINT(84),1)
        ICOLUP(2,I)=MCT(I+MINT(84),2)
        DO 170 J=1,5
          PUP(J,I)=P(I+MINT(84),J)
  170   CONTINUE
        VTIMUP(I)=V(I,5)
        SPINUP(I)=9D0
  180 CONTINUE
 
C...Optionally write out event to disk. Minimal size for time/spin fields.
      IF(MSTP(162).GT.0) THEN
        WRITE(MSTP(162),5200) NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP
        DO 190 I=1,NUP
          IF(VTIMUP(I).EQ.0D0) THEN
            WRITE(MSTP(162),5300) IDUP(I),ISTUP(I),MOTHUP(1,I),
     &      MOTHUP(2,I),ICOLUP(1,I),ICOLUP(2,I),(PUP(J,I),J=1,5),
     &      ' 0. 9.'
          ELSE
            WRITE(MSTP(162),5400) IDUP(I),ISTUP(I),MOTHUP(1,I),
     &      MOTHUP(2,I),ICOLUP(1,I),ICOLUP(2,I),(PUP(J,I),J=1,5),
     &      VTIMUP(I),' 9.'
          ENDIF
  190   CONTINUE

C...Optional extra line with parton-density information.
        IF(MSTP(165).GE.1) WRITE(MSTP(162),5500) MSTI(15),MSTI(16),
     &  PARI(33),PARI(34),PARI(23),PARI(29),PARI(30) 
      ENDIF
 
C...Error messages and other print formats.
 5100 FORMAT(1X,'Error: no subprocess switched on.'/
     &1X,'Execution stopped.')
 5200 FORMAT(1P,2I6,4E14.6)
 5300 FORMAT(1P,I8,5I5,5E18.10,A6)
 5400 FORMAT(1P,I8,5I5,5E18.10,E12.4,A3)
 5500 FORMAT(1P,'#pdf ',2I5,5E18.10)
 
      RETURN
      END
