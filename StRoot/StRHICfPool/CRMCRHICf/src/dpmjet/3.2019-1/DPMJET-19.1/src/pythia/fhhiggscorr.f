cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...FHHIGGSCORR
C...Dummy function, to be removed when FEYNHIGGS is to be linked.
 
      SUBROUTINE FHHIGGSCORR(IERR, RMHIGG, SAEFF, UHIGGS)
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...FeynHiggs variables
      DOUBLE PRECISION RMHIGG(4)
      COMPLEX*16 SAEFF, UHIGGS(3,3)
C unvar      COMPLEX*16 DMU,
C unvar     &     AE33, AU33, AD33, AE22, AU22, AD22, AE11, AU11, AD11,
C unvar     &     DM1, DM2, DM3

C...Commonblocks.
      include 'inc/pydat1'
 
C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      CALL PYSTOP(103)
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link FEYNHIGGS correctly.'/
     &1X,'Dummy routine FHSETPARA in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
      RETURN
      END
