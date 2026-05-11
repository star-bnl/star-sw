cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYNAME
C...Gives the particle/parton name as a character string.
 
      SUBROUTINE PYNAME(KF,CHAU)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYCOMP

C...Commonblocks.
      include 'inc/pydat1'
      include 'inc/pydat2'
      include 'inc/pydat4'

C...Local character variable.
      CHARACTER CHAU*16
 
C...Read out code with distinction particle/antiparticle.
      CHAU=' '
      KC=PYCOMP(KF)
      IF(KC.NE.0) CHAU=CHAF(KC,(3-SIGN(1,KF))/2)
 
 
      RETURN
      END
