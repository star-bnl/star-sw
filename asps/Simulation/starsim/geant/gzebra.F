* $Id: gzebra.F,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: gzebra.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/02/27 10:15:47  nevski
*  first working release
*
* Revision 1.8  2000/01/09 21:30:48  nevski
* fortran-style memgetf
*
      SUBROUTINE GZEBRA(NZEB)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to initialise ZEBRA store (//)                   *
C.    *                                                                *
C.    *    ==>Called by : <USER>                                       *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    *       Modifications:                                           *
C.    *   PN: Ellastic zebra inspired by V.Perevozchikov               *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
       INTEGER I1,I2,Itry,LL,KWW,MEMGETF,LOCF,NZEB
C.
C.    ------------------------------------------------------------------
C.
       NZEBRA = abs(NZEB)
       NZEBRA = max(NZEBRA,1000000)
       I1     = LOCF   (IQ)
       Do iTry=0,5
          I2  = MEMGETF (NZEBRA+1000)
          IF (I2.GT.I1) GO TO 11
          NZEBRA = (NZEBRA/15000)*10000
          PRINT *,'* GZEBRA: requested memory size reduced to ',
     >               Nzebra,' *'
       enddo
       PRINT *,' iTRY,NZEBRA,I1,I2 =', iTRY,NZEBRA,I1,I2
       STOP    ' GZEBRA: FATAL - CAN NOT ALLOCATE MEMORY '
C
  11   LL     = max    (I2-I1,0)
       NZEBRA = LL + NZEBRA
       KWW    = LL + 100
C
       CALL MZSTOR (IXSTOR,'/GCBANK/',' ',FENDQ,LQ,LR1,WS,
     >                                    LQ(KWW+100),LQ(NZEBRA-100))
       IF (IDEBUG.GE.3) CALL MZLOGL(IXSTOR,0)
       I1     = KWBANK
       CALL MZWORK (IXSTOR,LQ(I1),LQ(KWW),5)
       CALL GWORK  (KWBANK-100)
C
      END

