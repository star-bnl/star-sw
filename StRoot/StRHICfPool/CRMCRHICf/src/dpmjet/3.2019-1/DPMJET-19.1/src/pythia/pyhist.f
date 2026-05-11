cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYHIST
C...Prints and resets all histograms.
 
      SUBROUTINE PYHIST
 
C...Double precision declaration.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
C...Commonblock.
      include 'inc/pybins'
 
C...Loop over histograms, print and reset used ones.
      DO 100 ID=1,IHIST(1)
        IS=INDX(ID)
        IF(IS.NE.0.AND.NINT(BIN(IS+5)).GT.0) THEN
          CALL PYPLOT(ID)
          CALL PYNULL(ID)
        ENDIF
  100 CONTINUE
 
      RETURN
      END
