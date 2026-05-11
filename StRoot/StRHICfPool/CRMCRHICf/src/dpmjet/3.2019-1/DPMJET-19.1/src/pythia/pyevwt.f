cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYEVWT
C...Dummy routine, which the user can replace in order to multiply the
C...standard PYTHIA differential cross-section by a process- and
C...kinematics-dependent factor WTXS. For MSTP(142)=1 this corresponds
C...to generation of weighted events, with weight 1/WTXS, while for
C...MSTP(142)=2 it corresponds to a modification of the underlying
C...physics.
 
      SUBROUTINE PYEVWT(WTXS)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pydat1'
      include 'inc/pyint1'
      include 'inc/pyint2'
 
C C...Set default weight for WTXS.
C       WTXS=1D0
 
C C...Read out subprocess number.
C       ISUB=MINT(1)
C       ISTSB=ISET(ISUB)
 
C C...Read out tau, y*, cos(theta), tau' (where defined, else =0).
C       TAU=VINT(21)
C       YST=VINT(22)
C C unvar      CTH=0D0
C       IF(ISTSB.EQ.2.OR.ISTSB.EQ.4) CTH=VINT(23)
C       TAUP=0D0
C       IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUP=VINT(26)
 
C C...Read out x_1, x_2, x_F, shat, that, uhat, p_T^2.
C       X1=VINT(41)
C       X2=VINT(42)
C       XF=X1-X2
C       SHAT=VINT(44)
C       THAT=VINT(45)
C       UHAT=VINT(46)
C       PT2=VINT(48)
 
C...Modifications by user to be put here.
 
C...Stop program if this routine is ever called.
C...You should not copy these lines to your own routine.
      WRITE(MSTU(11),5000)
      CALL PYSTOP(4)
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link your PYEVWT routine ',
     &'correctly.'/1X,'Dummy routine in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
