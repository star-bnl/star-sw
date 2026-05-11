cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYKCUT
C...Dummy routine, which the user can replace in order to make cuts on
C...the kinematics on the parton level before the matrix elements are
C...evaluated and the event is generated. The cross-section estimates
C...will automatically take these cuts into account, so the given
C...values are for the allowed phase space region only. MCUT=0 means
C...that the event has passed the cuts, MCUT=1 that it has failed.
 
      SUBROUTINE PYKCUT(MCUT)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pydat1'
      include 'inc/pyint1'
      include 'inc/pyint2'
 
C...Set default value (accepting event) for MCUT.
C       MCUT=0
 
C C...Read out subprocess number.
C       ISUB=MINT(1)
C       ISTSB=ISET(ISUB)
 
C C...Read out tau, y*, cos(theta), tau' (where defined, else =0).
C       TAU=VINT(21)
C       YST=VINT(22)
C       CTH=0D0
C       IF(ISTSB.EQ.2.OR.ISTSB.EQ.4) CTH=VINT(23)
C       TAUP=0D0
C       IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUP=VINT(26)
 
C C...Calculate x_1, x_2, x_F.
C       IF(ISTSB.LE.2.OR.ISTSB.GE.5) THEN
C         X1=SQRT(TAU)*EXP(YST)
C         X2=SQRT(TAU)*EXP(-YST)
C       ELSE
C         X1=SQRT(TAUP)*EXP(YST)
C         X2=SQRT(TAUP)*EXP(-YST)
C       ENDIF
C C unvar      XF=X1-X2
 
C C...Calculate shat, that, uhat, p_T^2.
C       SHAT=TAU*VINT(2)
C       SQM3=VINT(63)
C       SQM4=VINT(64)
C       RM3=SQM3/SHAT
C       RM4=SQM4/SHAT
C       BE34=SQRT(MAX(0D0,(1D0-RM3-RM4)**2-4D0*RM3*RM4))
C       RPTS=4D0*VINT(71)**2/SHAT
C       BE34L=SQRT(MAX(0D0,(1D0-RM3-RM4)**2-4D0*RM3*RM4-RPTS))
C       RM34=2D0*RM3*RM4
C C unvar      RSQM=1D0+RM34
C       RTHM=(4D0*RM3*RM4+RPTS)/(1D0-RM3-RM4+BE34L)
C unvar      THAT=-0.5D0*SHAT*MAX(RTHM,1D0-RM3-RM4-BE34*CTH)
C unvar      UHAT=-0.5D0*SHAT*MAX(RTHM,1D0-RM3-RM4+BE34*CTH)
C unvar      PT2=MAX(VINT(71)**2,0.25D0*SHAT*BE34**2*(1D0-CTH**2))
 
C...Decisions by user to be put here.
 
C...Stop program if this routine is ever called.
C...You should not copy these lines to your own routine.
      WRITE(MSTU(11),5000)
      CALL PYSTOP(6)
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link your PYKCUT routine ',
     &'correctly.'/1X,'Dummy routine in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
