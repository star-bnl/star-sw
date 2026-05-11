cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYMRUN
C...Gives the running, current-algebra mass of a d, u, s, c or b quark,
C...for Higgs couplings. Everything else sent on to PYMASS.
 
      DOUBLE PRECISION FUNCTION PYMRUN(KF,Q2)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pydat1'
      include 'inc/pydat2'
      include 'inc/pypars'
 
C...Most masses not handled here.
      KFA=ABS(KF)
      IF(KFA.EQ.0.OR.KFA.GT.6) THEN
        PYMRUN=PYMASS(KF)
 
C...Current-algebra masses, but no Q2 dependence.
      ELSEIF(MSTP(37).NE.1.OR.MSTP(2).LE.0) THEN
        PYMRUN=PARF(90+KFA)
 
C...Running current-algebra masses.
      ELSE
C unvar        AS=PYALPS(Q2)
        PYMRUN=PARF(90+KFA)*
     &  (LOG(MAX(4D0,PARP(37)**2*PARF(90+KFA)**2/PARU(117)**2))/
     &  LOG(MAX(4D0,Q2/PARU(117)**2)))**(12D0/(33D0-2D0*MSTU(118)))
      ENDIF
 
      RETURN
      END
