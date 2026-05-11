cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYSTOP
C...Allows users to handle STOP statemens
 
      SUBROUTINE PYSTOP(MCOD)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pydat1'
 
C...Write message, then stop
      WRITE(MSTU(11),5000) MCOD
      STOP

 
C...Formats for output.
 5000 FORMAT(/5X,'PYSTOP called with code: ',I4)
      END
