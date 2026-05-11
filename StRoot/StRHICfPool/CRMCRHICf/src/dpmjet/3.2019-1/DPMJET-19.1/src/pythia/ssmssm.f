cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...VISAJE
C...Dummy function, to be removed when ISAJET (ISASUSY) is to be linked.
 
      CHARACTER*40 FUNCTION VISAJE()
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...Commonblocks.
      include 'inc/pydat1'
 
C...Assign default value.
      VISAJE='Undefined'
 
C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      CALL PYSTOP(110)
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link ISAJET correctly.'/
     &1X,'Dummy function VISAJE in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
 
 
C...SSMSSM
C...Dummy function, to be removed when ISAJET (ISASUSY) is to be linked.
 
      SUBROUTINE SSMSSM(RDUM1,RDUM2,RDUM3,RDUM4,RDUM5,RDUM6,RDUM7,
     &RDUM8,RDUM9,RDUM10,RDUM11,RDUM12,RDUM13,RDUM14,RDUM15,RDUM16,
     &RDUM17,RDUM18,RDUM19,RDUM20,RDUM21,RDUM22,RDUM23,RDUM24,RDUM25,
     &IDUM1,IDUM2)
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      REAL RDUM1,RDUM2,RDUM3,RDUM4,RDUM5,RDUM6,RDUM7,RDUM8,RDUM9,
     &RDUM10,RDUM11,RDUM12,RDUM13,RDUM14,RDUM15,RDUM16,RDUM17,RDUM18,
     &RDUM19,RDUM20,RDUM21,RDUM22,RDUM23,RDUM24,RDUM25
C...Commonblocks.
      include 'inc/pydat1'
 
C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      CALL PYSTOP(110)
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link ISAJET correctly.'/
     &1X,'Dummy routine SSMSSM in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
      RETURN
      END
