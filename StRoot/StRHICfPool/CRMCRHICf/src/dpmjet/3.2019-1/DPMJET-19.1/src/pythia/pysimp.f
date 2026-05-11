cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYSIMP
C...Simpson formula for an integral.
 
      DOUBLE PRECISION FUNCTION PYSIMP(Y,X0,X1,N)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...Local variables.
      DOUBLE PRECISION Y,X0,X1,H,S
      DIMENSION Y(0:N)
 
      S=0D0
      H=(X1-X0)/N
      DO 100 I=0,N-2,2
        S=S+Y(I)+4D0*Y(I+1)+Y(I+2)
  100 CONTINUE
      PYSIMP=S*H/3D0
 
      RETURN
      END
