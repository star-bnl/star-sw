cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYCKBD
C...Check that BLOCK DATA PYDATA has been loaded.
C...Should not be required, except that some compilers/linkers
C...are pretty buggy in this respect.
 
      SUBROUTINE PYCKBD
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pyjets'
      include 'inc/pydat1'
      include 'inc/pydat2'
      include 'inc/pydat3'
      include 'inc/pysubs'
      include 'inc/pypars'
 
C...Check a few variables to see they have been sensibly initialized.
      IF(MSTU(4).LT.10.OR.MSTU(4).GT.900000.OR.PMAS(2,1).LT.0.001D0
     &.OR.PMAS(2,1).GT.1D0.OR.CKIN(5).LT.0.01D0.OR.MSTP(1).LT.1.OR.
     &MSTP(1).GT.5) THEN
C...If not, abort the run right away.
        WRITE(*,*) 'Fatal error: BLOCKDATA PYDATA has not been loaded!'
        WRITE(*,*) 'The program execution is stopped now!'
        CALL PYSTOP(8)
      ENDIF
 
      RETURN
      END
