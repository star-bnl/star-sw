cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYVETO
C...Interface to UPVETO, which allows user to veto event generation
C...on the parton level, after parton showers but before multiple
C...interactions, beam remnants and hadronization is added.
 
      SUBROUTINE PYVETO(IVETO)
 
C...All real arithmetic in double precision.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
 
C...PYTHIA commonblocks.
      include 'inc/pyjets'
      include 'inc/pypars'
      include 'inc/pydat1'
      include 'inc/pyint1'

C...HEPEVT commonblock.
      include 'inc/hepevt'

C...Local array.
      DIMENSION IRESO(100)
 
C...Define longitudinal boost from initiator rest frame to cm frame.
      GAMMA=0.5D0*(VINT(141)+VINT(142))/SQRT(VINT(141)*VINT(142))
      GABEZ=0.5D0*(VINT(141)-VINT(142))/SQRT(VINT(141)*VINT(142))

C...Presentation is different if using pT-ordered shower
      IF(MINT(35).EQ.3) THEN
        GAMMA=1D0
        GABEZ=0D0
      ENDIF

C... Reset counters.
      NEVHEP=0
      NHEP=0
      NRESO=0
      
C...Oth pass: identify beam and incoming partons
      DO 140 I=MINT(83)+1,MINT(83)+6
        ISTORE=0
        IF(K(I,2).EQ.94) THEN

        ELSE
          NRESO=NRESO+1
          IRESO(NRESO)=I
          IMOTH=K(I,3)
        ENDIF
 140  CONTINUE

C...First pass: identify final locations of resonances
C...and of their daughters before showering.
      DO 150 I=MINT(84)+3,N
        ISTORE=0
        IMOTH=0
 
C...Skip shower CM frame documentation lines.
        IF(K(I,2).EQ.94) THEN
 
C...  Store a new intermediate product, when mother in documentation.
        ELSEIF(MSTP(128).EQ.0.AND.K(I,3).GT.MINT(83)+6.AND.
     &  K(I,3).LE.MINT(84)) THEN
          ISTORE=1
          NHEP=NHEP+1
          II=NHEP
          NRESO=NRESO+1
          IRESO(NRESO)=I
          IMOTH=MAX(0,K(K(I,3),3)-(MINT(83)+6))
 
C...  Store a new intermediate product, when mother in main section.
        ELSEIF(MSTP(128).EQ.1.AND.K(I-MINT(84)+MINT(83)+4,1).EQ.21.AND.
     &  K(I-MINT(84)+MINT(83)+4,2).EQ.K(I,2)) THEN
          ISTORE=1
          NHEP=NHEP+1
          II=NHEP
          NRESO=NRESO+1
          IRESO(NRESO)=I
          IMOTH=MAX(0,K(I-MINT(84)+MINT(83)+4,3)-(MINT(83)+6))
        ENDIF
  
        IF(ISTORE.EQ.1) THEN
C...Copy parton info, boosting momenta along z axis to cm frame.
          ISTHEP(II)=2
          IDHEP(II)=K(I,2)
          PHEP(1,II)=P(I,1)
          PHEP(2,II)=P(I,2)
          PHEP(3,II)=GAMMA*P(I,3)+GABEZ*P(I,4)
          PHEP(4,II)=GAMMA*P(I,4)+GABEZ*P(I,3)
          PHEP(5,II)=P(I,5)
C...Store one mother. Rest of history and vertex info zeroed.
          JMOHEP(1,II)=IMOTH
          JMOHEP(2,II)=0
          JDAHEP(1,II)=0
          JDAHEP(2,II)=0
          VHEP(1,II)=0D0
          VHEP(2,II)=0D0
          VHEP(3,II)=0D0
          VHEP(4,II)=0D0
        ENDIF
 150  CONTINUE

C...Second pass: identify current set of "final" partons.
      DO 200 I=MINT(84)+3,N
        ISTORE=0
        IMOTH=0
 
C...Store a final parton.
        IF(K(I,1).GE.1.AND.K(I,1).LE.10) THEN
          ISTORE=1
          NHEP=NHEP+1
          II=NHEP
C..Trace it back through shower, to check if from documented particle.
          IHIST=I
          ISAVE=IHIST
  160     CONTINUE
          IF(IHIST.GT.MINT(84)) THEN
            IF(K(IHIST,2).EQ.94) IHIST=K(IHIST,3)+(ISAVE-1-IHIST)
            DO 170 IRI=1,NRESO
              IF(IHIST.EQ.IRESO(IRI)) IMOTH=IRI
  170       CONTINUE
            ISAVE=IHIST
            IHIST=K(IHIST,3)
            IF(IMOTH.EQ.0) GOTO 160
            IMOTH=MAX(0,IMOTH-6)
          ELSEIF(IHIST.LE.4) THEN
            IF(IHIST.EQ.1.OR.IHIST.EQ.2) THEN
              ISTORE=0
              NHEP=NHEP-1
            ELSE
              IMOTH=0
            ENDIF
          ENDIF
        ENDIF
 
        IF(ISTORE.EQ.1) THEN
C...Copy parton info, boosting momenta along z axis to cm frame.
          ISTHEP(II)=1
          IDHEP(II)=K(I,2)
          PHEP(1,II)=P(I,1)
          PHEP(2,II)=P(I,2)
          PHEP(3,II)=GAMMA*P(I,3)+GABEZ*P(I,4)
          PHEP(4,II)=GAMMA*P(I,4)+GABEZ*P(I,3)
          PHEP(5,II)=P(I,5)
C...Store one mother. Rest of history and vertex info zeroed.
          JMOHEP(1,II)=IMOTH
          JMOHEP(2,II)=0
          JDAHEP(1,II)=0
          JDAHEP(2,II)=0
          VHEP(1,II)=0D0
          VHEP(2,II)=0D0
          VHEP(3,II)=0D0
          VHEP(4,II)=0D0
        ENDIF
  200 CONTINUE
C...Call user-written routine to decide whether to keep events.
      CALL UPVETO(IVETO)
      RETURN
      END
