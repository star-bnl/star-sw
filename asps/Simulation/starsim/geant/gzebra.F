* $Id: gzebra.F,v 1.5 2019/10/03 22:11:16 jwebb Exp $
* $Log: gzebra.F,v $
* Revision 1.5  2019/10/03 22:11:16  jwebb
* Revert back to 20MW.
*
* Revision 1.4  2019/10/01 20:32:46  jwebb
*
* Increase memory footprint of starsim, to mitigate #3395/3396.
*
* Revision 1.3  2018/11/26 19:47:47  perev
* Increase Zebra to 20M words
*
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
       common/GCBANK/IGC(20 000 000),IGCZ

       INTEGER Itry,KWW,MEMGETF,LOCF,NZEB
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
C
      END

