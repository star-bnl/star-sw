cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYFACT
C...Multiplies histogram contents by factor.
 
      SUBROUTINE PYFACT(ID,F)
 
C...Double precision declaration.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
C...Commonblock.
      include 'inc/pybins'
 
C...Find initial address in memory. Multiply all contents bins.
      IF(ID.LE.0.OR.ID.GT.IHIST(1)) CALL PYERRM(28,
     &'(PYFACT:) not allowed histogram number')
      IS=INDX(ID)
      IF(IS.EQ.0) CALL PYERRM(28,
     &'(PYFACT:) scaling unbooked histogram')
      DO 100 IX=IS+6,IS+8+NINT(BIN(IS+1))
        BIN(IX)=F*BIN(IX)
  100 CONTINUE
 
      RETURN
      END
