* $Id: gzebra.F,v 1.2 2018/11/19 21:23:41 perev Exp $
* $Log: gzebra.F,v $
* Revision 1.2  2018/11/19 21:23:41  perev
* 64bits Use array in common instead of malloc
*
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
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
       common/GCBANK/IGC(5000000),IGCZ

       INTEGER II1,II2,Itry,KWW,MEMGETF,LOCF,NZEB
       INTEGER *8 I1/0/,I2/0/
       equivalence (II1,I1),(II2,I2)
C.
C.    ------------------------------------------------------------------
C.
#if 1
      NZEBRA = locf(IGCZ)-locf(iq(1)) 
      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/GCBANK/',' ',FENDQ,LQ,LR1,WS,LQ(KWWORK+100)
     +            ,LQ(NZEBRA-30))
      CALL MZLOGL(IXSTOR,0)
#endif
#if 0
       NZEBRA = abs(NZEB)
       NZEBRA = max(NZEBRA,1000000)
       II1     = LOCF   (IQ)
       II2  = MEMGETF (NZEBRA+1000)
       PRINT *,' NZEBRA,I1,I2 =', NZEBRA,I1,I2
       IF (I2.LT.I1) call ZFATAL
C
  11   LL     = (I2-I1)
       if (ll<0) then
          PRINT *,' Too big Q,', LL
          call zfatal
       endif
       NZEBRA = LL + NZEBRA
       KWW    = LL + 100
C
       CALL MZSTOR (IXSTOR,'/GCBANK/',' ',FENDQ,LQ,LR1,WS,
     >                                    LQ(KWW+100),LQ(NZEBRA-100))
       IF (IDEBUG.GE.3) CALL MZLOGL(IXSTOR,0)
       I1     = KWBANK
       CALL MZWORK (IXSTOR,LQ(KWBANK),LQ(KWW),5)
       CALL GWORK  (KWBANK-100)
#endif
C
      END

