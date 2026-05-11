cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYLAMF
C...The standard lambda function.
 
      DOUBLE PRECISION FUNCTION PYLAMF(X,Y,Z)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...Local variables.
      DOUBLE PRECISION X,Y,Z
 
      PYLAMF=(X-(Y+Z))**2-4D0*Y*Z
      IF(PYLAMF.LT.0D0) PYLAMF=0D0
 
      RETURN
      END
