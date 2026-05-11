cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYCHGE
C...Gives three times the charge for a particle/parton.
 
      INTEGER FUNCTION PYCHGE(KF)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYCOMP
C...Commonblocks.
      include 'inc/pydat2'
 
C...Read out charge and change sign for antiparticle.
      PYCHGE=0
      KC=PYCOMP(KF)
      IF(KC.NE.0) PYCHGE=KCHG(KC,1)*SIGN(1,KF)
 
      RETURN
      END
