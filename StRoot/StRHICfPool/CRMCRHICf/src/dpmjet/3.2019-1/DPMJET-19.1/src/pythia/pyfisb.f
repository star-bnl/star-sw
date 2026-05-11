cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYFISB
C...Auxiliary routine to PYFINT for SUSY Higgs calculations.
 
      DOUBLE PRECISION FUNCTION PYFISB(X)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
C...Commonblock.
      include 'inc/pyints'
 
      PYFISB = LOG(ABS(X*XXM(2)+(1-X)*XXM(3)-X*(1-X)*XXM(1))/
     &(X*(XXM(2)-XXM(3))+XXM(3)))
 
      RETURN
      END
