C...main53.f is a part of the PYTHIA event generator.
C...Copyright (C) 2008 Torbjorn Sjostrand.
C...PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
C...Please respect the MCnet Guidelines, see GUIDELINES for details.

C...Store Pythia6 parton-level events according to Les Houches Accord,
C...using the Les Houches Event File format. The files can then be used 
C...as input for hadron-level event simulation in Pythia8, see main12.cc.

C...Note: you need to create two temporary files for MSTP(161) and MSTP(162). 
C...The final call to PYLHEF will pack them into a standard-compliant 
C...Les Houches Event File on your unit MSTP(163), and erase the two
C...temporary files (unless you set MSTP(164)=1).

C----------------------------------------------------------------------------

C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER PYK,PYCHGE,PYCOMP

C...EXTERNAL statement links PYDATA on most machines.
      EXTERNAL PYDATA

C...Commonblocks.
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)

C...Number of events.
      NEV=100

C...Event kind.
      MSEL=6

C...Temporary files for initialization/event output.
      MSTP(161)=21
      OPEN(21,FILE='ttbar.init',STATUS='unknown')
      MSTP(162)=22
      OPEN(22,FILE='ttbar.evnt',STATUS='unknown')

C...Final Les Houches Event File, obtained by combining above two.
      MSTP(163)=23
      OPEN(23,FILE='ttbar.lhe',STATUS='unknown')

C..Also save the optional parton-density information.
      MSTP(165)=1

C...Initialize.
      CALL PYINIT('CMS','P','PBAR',1960D0)

C...Event loop. List first few events.
      DO 200 IEV=1,NEV
        CALL PYUPEV
        IF(IEV.LE.2) THEN 
          CALL PYLIST(2)
          CALL PYLIST(7)
        ENDIF 
 200  CONTINUE

C...Final statistics.
      CALL PYSTAT(1)
      CALL PYUPIN

C...Produce final Les Houches Event File.
      CALL PYLHEF

      END

