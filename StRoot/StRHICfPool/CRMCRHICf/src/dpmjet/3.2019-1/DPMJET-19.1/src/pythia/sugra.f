cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
 
 
C...SUGRA
C...Dummy routine, to be removed when ISAJET (ISASUSY) is to be linked.
 
      SUBROUTINE SUGRA(MZERO,MHLF,AZERO,TANB,SGNMU,MTOP,IMODL)
       IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      REAL MZERO,MHLF,AZERO,TANB,SGNMU,MTOP
      INTEGER IMODL
C...Commonblocks.
      include 'inc/pydat1'
 
C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      CALL PYSTOP(110)
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link ISAJET correctly.'/
     &1X,'Dummy routine SUGRA in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
