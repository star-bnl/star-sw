cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYMEMX
C...Generates maximum ME weight in some initial-state showers.
C...Inparameter MECOR: kind of hard scattering process
C...Outparameter WTFF: maximum weight for fermion -> fermion
C...             WTGF: maximum weight for gluon/photon -> fermion
C...             WTFG: maximum weight for fermion -> gluon/photon
C...             WTGG: maximum weight for gluon -> gluon
 
      SUBROUTINE PYMEMX(MECOR,WTFF,WTGF,WTFG,WTGG)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pyjets'
      include 'inc/pydat1'
      include 'inc/pypars'
      include 'inc/pyint1'
      include 'inc/pyint2'
 
C...Default maximum weight.
      WTFF=1D0
      WTGF=1D0
      WTFG=1D0
      WTGG=1D0
 
C...Select maximum weight by process.
      IF(MECOR.EQ.1) THEN
        WTFF=1D0
        WTGF=3D0
      ELSEIF(MECOR.EQ.2) THEN
        WTFG=1D0
        WTGG=1D0
      ENDIF
 
      RETURN
      END
