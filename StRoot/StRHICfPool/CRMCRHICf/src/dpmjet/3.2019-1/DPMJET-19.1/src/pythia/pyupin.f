cc ------------ dpmjet3.4 - authors: S.Roesler, R.Engel, J.Ranft -------
cc -------- phojet1.12-40 - authors: S.Roesler, R.Engel, J.Ranft -------
cc                                                      - oct'13 -------
cc ----------- pythia-6.4 - authors: Torbjorn Sjostrand, Lund'10 -------
cc ---------------------------------------------------------------------
cc                                  converted for use with FLUKA -------
cc                                                      - oct'13 -------
 
C...PYUPIN
C...Fills the HEPRUP commonblock with info on incoming beams and allowed
C...processes, and optionally stores that information on file.
 
      SUBROUTINE PYUPIN
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
 
C...Commonblocks.
      include 'inc/pyjets'
      include 'inc/pysubs'
      include 'inc/pypars'
      include 'inc/pyint5'
 
C...User process initialization commonblock.
      include 'inc/heprup'
 
C...Store info on incoming beams.
      IDBMUP(1)=K(1,2)
      IDBMUP(2)=K(2,2)
      EBMUP(1)=P(1,4)
      EBMUP(2)=P(2,4)
      PDFGUP(1)=0
      PDFGUP(2)=0
      PDFSUP(1)=MSTP(51)
      PDFSUP(2)=MSTP(51)
 
C...Event weighting strategy.
      IDWTUP=3
 
C...Info on individual processes.
      NPRUP=0
      DO 100 ISUB=1,500
        IF(MSUB(ISUB).EQ.1) THEN
          NPRUP=NPRUP+1
          XSECUP(NPRUP)=1D9*XSEC(ISUB,3)
          XERRUP(NPRUP)=XSECUP(NPRUP)/SQRT(MAX(1D0,DBLE(NGEN(ISUB,3))))
          XMAXUP(NPRUP)=1D0
          LPRUP(NPRUP)=ISUB
        ENDIF
  100 CONTINUE
 
C...Write info to file.
      IF(MSTP(161).GT.0) THEN
        WRITE(MSTP(161),5100) IDBMUP(1),IDBMUP(2),EBMUP(1),EBMUP(2),
     &  PDFGUP(1),PDFGUP(2),PDFSUP(1),PDFSUP(2),IDWTUP,NPRUP
        DO 110 IPR=1,NPRUP
          WRITE(MSTP(161),5200) XSECUP(IPR),XERRUP(IPR),XMAXUP(IPR),
     &    LPRUP(IPR)
  110   CONTINUE
      ENDIF
 
C...Formats for printout.
 5100 FORMAT(1P,2I8,2E14.6,6I6)
 5200 FORMAT(1P,3E14.6,I6)
 
      RETURN
      END
