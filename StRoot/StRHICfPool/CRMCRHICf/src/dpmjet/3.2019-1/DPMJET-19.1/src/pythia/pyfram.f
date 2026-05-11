cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYFRAM
C...Performs transformations between different coordinate frames.
 
      SUBROUTINE PYFRAM(IFRAME)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pydat1'
      include 'inc/pypars'
      include 'inc/pyint1'
 
C...Check that transformation can and should be done.
      IF(IFRAME.EQ.1.OR.IFRAME.EQ.2.OR.(IFRAME.EQ.3.AND.
     &MINT(91).EQ.1)) THEN
        IF(IFRAME.EQ.MINT(6)) RETURN
      ELSE
        WRITE(MSTU(11),5000) IFRAME,MINT(6)
        RETURN
      ENDIF
 
      IF(MINT(6).EQ.1) THEN
C...Transform from fixed target or user specified frame to
C...overall CM frame.
        CALL PYROBO(0,0,0D0,0D0,-VINT(8),-VINT(9),-VINT(10))
        CALL PYROBO(0,0,0D0,-VINT(7),0D0,0D0,0D0)
        CALL PYROBO(0,0,-VINT(6),0D0,0D0,0D0,0D0)
      ELSEIF(MINT(6).EQ.3) THEN
C...Transform from hadronic CM frame in DIS to overall CM frame.
        CALL PYROBO(0,0,-VINT(221),-VINT(222),-VINT(223),-VINT(224),
     &  -VINT(225))
      ENDIF
 
      IF(IFRAME.EQ.1) THEN
C...Transform from overall CM frame to fixed target or user specified
C...frame.
        CALL PYROBO(0,0,VINT(6),VINT(7),VINT(8),VINT(9),VINT(10))
      ELSEIF(IFRAME.EQ.3) THEN
C...Transform from overall CM frame to hadronic CM frame in DIS.
        CALL PYROBO(0,0,0D0,0D0,VINT(223),VINT(224),VINT(225))
        CALL PYROBO(0,0,0D0,VINT(222),0D0,0D0,0D0)
        CALL PYROBO(0,0,VINT(221),0D0,0D0,0D0,0D0)
      ENDIF
 
C...Set information about new frame.
      MINT(6)=IFRAME
      MSTI(6)=IFRAME
 
 5000 FORMAT(1X,'Error: illegal values in subroutine PYFRAM.',1X,
     &'No transformation performed.'/1X,'IFRAME =',1X,I5,'; MINT(6) =',
     &1X,I5)
 
      RETURN
      END
