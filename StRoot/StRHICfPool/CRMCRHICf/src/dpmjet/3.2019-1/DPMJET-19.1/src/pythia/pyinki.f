cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYINKI
C...Sets up kinematics, including rotations and boosts to/from CM frame.
 
      SUBROUTINE PYINKI(MODKI)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...User process initialization commonblock.
      include 'inc/heprup'
 
C...Commonblocks.
      include 'inc/pyjets'
      include 'inc/pydat1'
      include 'inc/pydat2'
      include 'inc/pysubs'
      include 'inc/pypars'
      include 'inc/pyint1'
 
C...Set initial flavour state.
      N=2
      DO 100 I=1,2
        K(I,1)=1
        K(I,2)=MINT(10+I)
        IF(MINT(140+I).NE.0) K(I,2)=MINT(140+I)
  100 CONTINUE
 
C...Reset boost. Do kinematics for various cases.
      DO 110 J=6,10
        VINT(J)=0D0
  110 CONTINUE
 
C...Set up kinematics for events defined in CM frame.
      IF(MINT(111).EQ.1) THEN
        WIN=VINT(290)
        IF(MODKI.EQ.1) WIN=PARP(171)*VINT(290)
        S=WIN**2
        P(1,5)=VINT(3)
        P(2,5)=VINT(4)
        IF(MINT(141).NE.0) P(1,5)=VINT(303)
        IF(MINT(142).NE.0) P(2,5)=VINT(304)
        P(1,1)=0D0
        P(1,2)=0D0
        P(2,1)=0D0
        P(2,2)=0D0
        P(1,3)=SQRT(((S-P(1,5)**2-P(2,5)**2)**2-(2D0*P(1,5)*P(2,5))**2)/
     &  (4D0*S))
        P(2,3)=-P(1,3)
        P(1,4)=SQRT(P(1,3)**2+P(1,5)**2)
        P(2,4)=SQRT(P(2,3)**2+P(2,5)**2)
 
C...Set up kinematics for fixed target events.
      ELSEIF(MINT(111).EQ.2) THEN
        WIN=VINT(290)
        IF(MODKI.EQ.1) WIN=PARP(171)*VINT(290)
        P(1,5)=VINT(3)
        P(2,5)=VINT(4)
        IF(MINT(141).NE.0) P(1,5)=VINT(303)
        IF(MINT(142).NE.0) P(2,5)=VINT(304)
        P(1,1)=0D0
        P(1,2)=0D0
        P(2,1)=0D0
        P(2,2)=0D0
        P(1,3)=WIN
        P(1,4)=SQRT(P(1,3)**2+P(1,5)**2)
        P(2,3)=0D0
        P(2,4)=P(2,5)
        S=P(1,5)**2+P(2,5)**2+2D0*P(2,4)*P(1,4)
        VINT(10)=P(1,3)/(P(1,4)+P(2,4))
        CALL PYROBO(0,0,0D0,0D0,0D0,0D0,-VINT(10))
 
C...Set up kinematics for events in user-defined frame.
      ELSEIF(MINT(111).EQ.3) THEN
        P(1,5)=VINT(3)
        P(2,5)=VINT(4)
        IF(MINT(141).NE.0) P(1,5)=VINT(303)
        IF(MINT(142).NE.0) P(2,5)=VINT(304)
        P(1,4)=SQRT(P(1,1)**2+P(1,2)**2+P(1,3)**2+P(1,5)**2)
        P(2,4)=SQRT(P(2,1)**2+P(2,2)**2+P(2,3)**2+P(2,5)**2)
        DO 120 J=1,3
          VINT(7+J)=(P(1,J)+P(2,J))/(P(1,4)+P(2,4))
  120   CONTINUE
        CALL PYROBO(0,0,0D0,0D0,-VINT(8),-VINT(9),-VINT(10))
        VINT(7)=PYANGL(P(1,1),P(1,2))
        CALL PYROBO(0,0,0D0,-VINT(7),0D0,0D0,0D0)
        VINT(6)=PYANGL(P(1,3),P(1,1))
        CALL PYROBO(0,0,-VINT(6),0D0,0D0,0D0,0D0)
        S=P(1,5)**2+P(2,5)**2+2D0*(P(1,4)*P(2,4)-P(1,3)*P(2,3))
 
C...Set up kinematics for events with user-defined four-vectors.
      ELSEIF(MINT(111).EQ.4) THEN
        PMS1=P(1,4)**2-P(1,1)**2-P(1,2)**2-P(1,3)**2
        P(1,5)=SIGN(SQRT(ABS(PMS1)),PMS1)
        PMS2=P(2,4)**2-P(2,1)**2-P(2,2)**2-P(2,3)**2
        P(2,5)=SIGN(SQRT(ABS(PMS2)),PMS2)
        DO 130 J=1,3
          VINT(7+J)=(P(1,J)+P(2,J))/(P(1,4)+P(2,4))
  130   CONTINUE
        CALL PYROBO(0,0,0D0,0D0,-VINT(8),-VINT(9),-VINT(10))
        VINT(7)=PYANGL(P(1,1),P(1,2))
        CALL PYROBO(0,0,0D0,-VINT(7),0D0,0D0,0D0)
        VINT(6)=PYANGL(P(1,3),P(1,1))
        CALL PYROBO(0,0,-VINT(6),0D0,0D0,0D0,0D0)
        S=(P(1,4)+P(2,4))**2
 
C...Set up kinematics for events with user-defined five-vectors.
      ELSEIF(MINT(111).EQ.5) THEN
        DO 140 J=1,3
          VINT(7+J)=(P(1,J)+P(2,J))/(P(1,4)+P(2,4))
  140   CONTINUE
        CALL PYROBO(0,0,0D0,0D0,-VINT(8),-VINT(9),-VINT(10))
        VINT(7)=PYANGL(P(1,1),P(1,2))
        CALL PYROBO(0,0,0D0,-VINT(7),0D0,0D0,0D0)
        VINT(6)=PYANGL(P(1,3),P(1,1))
        CALL PYROBO(0,0,-VINT(6),0D0,0D0,0D0,0D0)
        S=(P(1,4)+P(2,4))**2
 
C...Set up kinematics for events with external user processes.
      ELSEIF(MINT(111).GE.11) THEN
        P(1,5)=VINT(3)
        P(2,5)=VINT(4)
        IF(MINT(141).NE.0) P(1,5)=VINT(303)
        IF(MINT(142).NE.0) P(2,5)=VINT(304)
        P(1,1)=0D0
        P(1,2)=0D0
        P(2,1)=0D0
        P(2,2)=0D0
        P(1,3)=SQRT(MAX(0D0,EBMUP(1)**2-P(1,5)**2))
        P(2,3)=-SQRT(MAX(0D0,EBMUP(2)**2-P(2,5)**2))
        P(1,4)=EBMUP(1)
        P(2,4)=EBMUP(2)
        VINT(10)=(P(1,3)+P(2,3))/(P(1,4)+P(2,4))
        CALL PYROBO(0,0,0D0,0D0,0D0,0D0,-VINT(10))
        S=(P(1,4)+P(2,4))**2
      ENDIF
 
C...Return or error for too low CM energy.
      IF(MODKI.EQ.1.AND.S.LT.PARP(2)**2) THEN
        IF(MSTP(172).LE.1) THEN
          CALL PYERRM(23,
     &    '(PYINKI:) too low invariant mass in this event')
        ELSE
          MSTI(61)=1
          RETURN
        ENDIF
      ENDIF
 
C...Save information on incoming particles.
      VINT(1)=SQRT(S)
      VINT(2)=S
      IF(MINT(111).GE.4) THEN
        IF(MINT(141).EQ.0) THEN
          VINT(3)=P(1,5)
          IF(MINT(11).EQ.22.AND.P(1,5).LT.0) VINT(307)=P(1,5)**2
        ELSE
          VINT(303)=P(1,5)
        ENDIF
        IF(MINT(142).EQ.0) THEN
          VINT(4)=P(2,5)
          IF(MINT(12).EQ.22.AND.P(2,5).LT.0) VINT(308)=P(2,5)**2
        ELSE
          VINT(304)=P(2,5)
        ENDIF
      ENDIF
      VINT(5)=P(1,3)
      IF(MODKI.EQ.0) VINT(289)=S
      DO 150 J=1,5
        V(1,J)=0D0
        V(2,J)=0D0
        VINT(290+J)=P(1,J)
        VINT(295+J)=P(2,J)
  150 CONTINUE
 
C...Store pT cut-off and related constants to be used in generation.
      IF(MODKI.EQ.0) VINT(285)=CKIN(3)
      IF(MSTP(82).LE.1) THEN
        PTMN=PARP(81)*(VINT(1)/PARP(89))**PARP(90)
      ELSE
        PTMN=PARP(82)*(VINT(1)/PARP(89))**PARP(90)
      ENDIF
      VINT(149)=4D0*PTMN**2/S
      VINT(154)=PTMN
 
      RETURN
      END
