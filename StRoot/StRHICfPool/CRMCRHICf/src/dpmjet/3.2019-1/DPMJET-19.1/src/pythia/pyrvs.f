cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYRVS
C...Interference function
 
      DOUBLE PRECISION FUNCTION PYRVS(X,Y,M1,W1,M2,W2)
 
      IMPLICIT NONE
      DOUBLE PRECISION X, Y, PYRVR, M1, M2, W1, W2
      PYRVS = PYRVR(X,M1,W1)*PYRVR(Y,M2,W2)*((X-M1**2)*(Y-M2**2)
     &     +W1*W2*M1*M2)
      RETURN
      END
