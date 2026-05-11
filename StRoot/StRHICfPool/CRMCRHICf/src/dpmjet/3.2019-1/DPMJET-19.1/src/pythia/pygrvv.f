cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYGRVV
C...Auxiliary for the GRV 94 parton distribution functions
C...for u and d valence and d-u sea.
C...Authors: M. Glueck, E. Reya and A. Vogt.
 
      DOUBLE PRECISION FUNCTION PYGRVV (X, N, AK, BK, A, B, C, D)
 
C...Double precision declaration.
      IMPLICIT DOUBLE PRECISION (A - Z)
 
C...Evaluation.
      DX = SQRT (X)
      PYGRVV = N * X**AK * (1D0+ A*X**BK + X * (B + C*DX)) *
     & (1D0- X)**D
 
      RETURN
      END
