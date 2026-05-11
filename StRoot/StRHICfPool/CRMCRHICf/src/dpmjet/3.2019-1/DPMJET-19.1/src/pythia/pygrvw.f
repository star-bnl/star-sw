cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYGRVW
C...Auxiliary for the GRV 94 parton distribution functions
C...for d+u sea and gluon.
C...Authors: M. Glueck, E. Reya and A. Vogt.
 
      DOUBLE PRECISION FUNCTION PYGRVW (X, S, AL, BE, AK, BK, A, B, C, 
     &                                  D, E, ES)
 
C...Double precision declaration.
      IMPLICIT DOUBLE PRECISION (A - Z)
 
C...Evaluation.
      LX = LOG (1D0/X)
      PYGRVW = (X**AK * (A + X * (B + X*C)) * LX**BK + S**AL
     &     * EXP (-E + SQRT (ES * S**BE * LX))) * (1D0- X)**D
 
      RETURN
      END
