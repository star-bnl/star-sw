cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYRVR
C...Breit-Wigner for resonance contributions
 
      DOUBLE PRECISION FUNCTION PYRVR(Mab2,RM,RW)
 
      IMPLICIT NONE
      DOUBLE PRECISION Mab2,RM,RW
      PYRVR = 1D0/((Mab2-RM**2)**2+RM**2*RW**2)
      RETURN
      END
