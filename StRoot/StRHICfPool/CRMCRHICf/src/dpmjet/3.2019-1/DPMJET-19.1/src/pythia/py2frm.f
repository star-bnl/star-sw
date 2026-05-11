cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PY2FRM
C...An interface from a two-fermion generator to include
C...parton showers and hadronization.
 
      SUBROUTINE PY2FRM(IRAD,ITAU,ICOM)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pyjets'
      include 'inc/pydat1'
C...Local arrays.
      DIMENSION IJOIN(2),INTAU(2)
 
C...Call PYHEPC to convert input from HEPEVT to PYJETS common.
      IF(ICOM.EQ.0) THEN
        MSTU(28)=0
        CALL PYHEPC(2)
      ENDIF
 
C...Loop through entries and pick up all final fermions/antifermions.
      I1=0
      I2=0
      DO 100 I=1,N
      IF(K(I,1).LE.0.OR.K(I,1).GT.10) GOTO 100
      KFA=ABS(K(I,2))
      IF((KFA.GE.1.AND.KFA.LE.6).OR.(KFA.GE.11.AND.KFA.LE.16)) THEN
        IF(K(I,2).GT.0) THEN
          IF(I1.EQ.0) THEN
            I1=I
          ELSE
            CALL PYERRM(16,'(PY2FRM:) more than one fermion')
          ENDIF
        ELSE
          IF(I2.EQ.0) THEN
            I2=I
          ELSE
            CALL PYERRM(16,'(PY2FRM:) more than one antifermion')
          ENDIF
        ENDIF
      ENDIF
  100 CONTINUE
 
C...Check that event is arranged according to conventions.
      IF(I1.EQ.0.OR.I2.EQ.0) THEN
        CALL PYERRM(16,'(PY2FRM:) event contains too few fermions')
      ENDIF
      IF(I2.LT.I1) THEN
        CALL PYERRM(6,'(PY2FRM:) fermions arranged in wrong order')
      ENDIF
 
C...Check whether fermion pair is quarks or leptons.
      IF(ABS(K(I1,2)).LT.10.AND.ABS(K(I2,2)).LT.10) THEN
        IQL12=1
      ELSEIF(ABS(K(I1,2)).GT.10.AND.ABS(K(I2,2)).GT.10) THEN
        IQL12=2
      ELSE
        CALL PYERRM(16,'(PY2FRM:) fermion pair inconsistent')
      ENDIF
 
C...Decide whether to allow or not photon radiation in showers.
      MSTJ(41)=2
      IF(IRAD.EQ.0) MSTJ(41)=1
 
C...Do colour joining and parton showers.
      IP1=I1
      IP2=I2
      IF(IQL12.EQ.1) THEN
        IJOIN(1)=IP1
        IJOIN(2)=IP2
        CALL PYJOIN(2,IJOIN)
      ENDIF
      IF(IQL12.EQ.1.OR.IRAD.EQ.1) THEN
        PM12S=(P(IP1,4)+P(IP2,4))**2-(P(IP1,1)+P(IP2,1))**2-
     &  (P(IP1,2)+P(IP2,2))**2-(P(IP1,3)+P(IP2,3))**2
        CALL PYSHOW(IP1,IP2,SQRT(MAX(0D0,PM12S)))
      ENDIF
 
C...Do fragmentation and decays. Possibly except tau decay.
      IF(ITAU.EQ.0) THEN
        NTAU=0
        DO 110 I=1,N
        IF(ABS(K(I,2)).EQ.15.AND.K(I,1).EQ.1) THEN
          NTAU=NTAU+1
          INTAU(NTAU)=I
          K(I,1)=11
        ENDIF
  110   CONTINUE
      ENDIF
      CALL PYEXEC
      IF(ITAU.EQ.0) THEN
        DO 120 I=1,NTAU
        K(INTAU(I),1)=1
  120   CONTINUE
      ENDIF
 
C...Call PYHEPC to convert output from PYJETS to HEPEVT common.
      IF(ICOM.EQ.0) THEN
        MSTU(28)=0
        CALL PYHEPC(1)
      ENDIF
 
      END
