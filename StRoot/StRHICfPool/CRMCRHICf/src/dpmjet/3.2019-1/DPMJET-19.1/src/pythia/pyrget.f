cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYRGET
C...Dumps the state of the random number generator on a file
C...for subsequent startup from this state onwards.
 
      SUBROUTINE PYRGET(LFN,MOVE)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pydatr'

C...Local character variable.
      CHARACTER CHERR*8
 
C...Backspace required number of records (or as many as there are).
      IF(MOVE.LT.0) THEN
        NBCK=MIN(MRPY(6),-MOVE)
        DO 100 IBCK=1,NBCK
          BACKSPACE(LFN,ERR=110,IOSTAT=IERR)
  100   CONTINUE
        MRPY(6)=MRPY(6)-NBCK
      ENDIF
 
C...Unformatted write on unit LFN.
      WRITE(LFN,ERR=110,IOSTAT=IERR) (MRPY(I1),I1=1,5),
     &(RRPY(I2),I2=1,100)
      MRPY(6)=MRPY(6)+1
      RETURN
 
C...Write error.
  110 WRITE(CHERR,'(I8)') IERR
      CALL PYERRM(18,'(PYRGET:) error when accessing file, IOSTAT ='//
     &CHERR)
 
      RETURN
      END
