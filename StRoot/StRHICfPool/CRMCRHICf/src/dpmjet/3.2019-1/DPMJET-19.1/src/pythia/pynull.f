cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYNULL
C...Resets bin contents of a histogram.
 
      SUBROUTINE PYNULL(ID)
 
C...Double precision declaration.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
C...Commonblock.
      include 'inc/pybins'
 
      IF(ID.LE.0.OR.ID.GT.IHIST(1)) RETURN
      IS=INDX(ID)
      IF(IS.EQ.0) RETURN
      DO 100 IX=IS+5,IS+8+NINT(BIN(IS+1))
        BIN(IX)=0D0
  100 CONTINUE
 
      RETURN
      END
