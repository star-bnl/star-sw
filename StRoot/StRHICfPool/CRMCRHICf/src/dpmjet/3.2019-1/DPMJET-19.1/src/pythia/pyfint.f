cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYFINT
C...Auxiliary routine to PYPOLE for SUSY Higgs calculations.
 
      DOUBLE PRECISION FUNCTION PYFINT(A,B,C)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblock.
      include 'inc/pyints'
 
C...Local variables.
      EXTERNAL PYFISB
      DOUBLE PRECISION PYFISB
 
      XXM(1)=A
      XXM(2)=B
      XXM(3)=C
      XLO=0D0
      XHI=1D0
      PYFINT  = PYGAUS(PYFISB,XLO,XHI,1D-3)
 
      RETURN
      END
