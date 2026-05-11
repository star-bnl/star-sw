cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYFILL
C...Fills entry in histogram.
 
      SUBROUTINE PYFILL(ID,X,W)
 
C...Double precision declaration.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
C...Commonblock.
      include 'inc/pybins'
 
C...Find initial address in memory. Increase number of entries.
      IF(ID.LE.0.OR.ID.GT.IHIST(1)) CALL PYERRM(28,
     &'(PYFILL:) not allowed histogram number')
      IS=INDX(ID)
      IF(IS.EQ.0) CALL PYERRM(28,
     &'(PYFILL:) filling unbooked histogram')
      BIN(IS+5)=BIN(IS+5)+1D0
 
C...Find bin in x, including under/overflow, and fill.
      IF(X.LT.BIN(IS+2)) THEN
        BIN(IS+6)=BIN(IS+6)+W
      ELSEIF(X.GE.BIN(IS+3)) THEN
        BIN(IS+8)=BIN(IS+8)+W
      ELSE
        BIN(IS+7)=BIN(IS+7)+W
        IX=(X-BIN(IS+2))/BIN(IS+4)
        IX=MAX(0,MIN(NINT(BIN(IS+1))-1,IX))
        BIN(IS+9+IX)=BIN(IS+9+IX)+W
      ENDIF
 
      RETURN
      END
