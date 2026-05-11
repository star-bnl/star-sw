cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYGAMM
C...Gives ordinary Gamma function Gamma(x) for positive, real arguments;
C...see M. Abramowitz, I. A. Stegun: Handbook of Mathematical Functions
C...(Dover, 1965) 6.1.36.
 
      DOUBLE PRECISION FUNCTION PYGAMM(X)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Local array and data.
      DIMENSION B(8)
      DATA B/-0.577191652D0,0.988205891D0,-0.897056937D0,0.918206857D0,
     &-0.756704078D0,0.482199394D0,-0.193527818D0,0.035868343D0/
 
      NX=INT(X)
      DX=X-NX
 
      PYGAMM=1D0
      DXP=1D0
      DO 100 I=1,8
        DXP=DXP*DX
        PYGAMM=PYGAMM+B(I)*DXP
  100 CONTINUE
      IF(X.LT.1D0) THEN
        PYGAMM=PYGAMM/X
      ELSE
        DO 110 IX=1,NX-1
          PYGAMM=(X-IX)*PYGAMM
  110   CONTINUE
      ENDIF
 
      RETURN
      END
