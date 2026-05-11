cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYERRM
C...Informs user of errors in program execution.
 
      SUBROUTINE PYERRM(MERR,CHMESS)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

C...Commonblocks.
      include 'inc/pyjets'
      include 'inc/pydat1'

C...Local character variable.
      CHARACTER CHMESS*(*)
 
C...Write first few warnings, then be silent.
      IF(MERR.LE.10) THEN
        MSTU(27)=MSTU(27)+1
        MSTU(28)=MERR
        IF(MSTU(25).EQ.1.AND.MSTU(27).LE.MSTU(26)) WRITE(MSTU(11),5000)
     &  MERR,MSTU(31),CHMESS
 
C...Write first few errors, then be silent or stop program.
      ELSEIF(MERR.LE.20) THEN
        IF(MSTU(29).EQ.0) MSTU(23)=MSTU(23)+1
        MSTU(30)=MSTU(30)+1
        MSTU(24)=MERR-10
        IF(MSTU(21).GE.1.AND.MSTU(23).LE.MSTU(22)) WRITE(MSTU(11),5100)
     &  MERR-10,MSTU(31),CHMESS
        IF(MSTU(21).GE.2.AND.MSTU(23).GT.MSTU(22)) THEN
          WRITE(MSTU(11),5100) MERR-10,MSTU(31),CHMESS
          WRITE(MSTU(11),5200)
          IF(MERR.NE.17) CALL PYLIST(2)
          CALL PYSTOP(3)
        ENDIF
 
C...Stop program in case of irreparable error.
      ELSE
        WRITE(MSTU(11),5300) MERR-20,MSTU(31),CHMESS
        CALL PYSTOP(3)
      ENDIF
 
C...Formats for output.
 5000 FORMAT(/5X,'Advisory warning type',I2,' given after',I9,
     &' PYEXEC calls:'/5X,A)
 5100 FORMAT(/5X,'Error type',I2,' has occured after',I9,
     &' PYEXEC calls:'/5X,A)
 5200 FORMAT(5X,'Execution will be stopped after listing of last ',
     &'event!')
 5300 FORMAT(/5X,'Fatal error type',I2,' has occured after',I9,
     &' PYEXEC calls:'/5X,A/5X,'Execution will now be stopped!')
 
      RETURN
      END
