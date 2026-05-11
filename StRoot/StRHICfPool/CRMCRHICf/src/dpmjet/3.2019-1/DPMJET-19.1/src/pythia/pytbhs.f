cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
      DOUBLE PRECISION FUNCTION PYTBHS(A,B)
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      DIMENSION A(4),B(4)
      DUM=A(4)*B(4)
      DO 100 ID=1,3
         DUM=DUM-A(ID)*B(ID)
  100 CONTINUE
      PYTBHS=DUM
      RETURN
      END
