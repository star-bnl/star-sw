*
* $Id: gzebra.F,v 1.1.1.1 2004/01/15 00:12:03 potekhin Exp $
*
* $Log: gzebra.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:03  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:33  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:11  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.19  by  S.Giani
*-- Author :
      SUBROUTINE GZEBRA(NZEB)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to initialise ZEBRA store (//)                   *
C.    *                                                                *
C.    *    ==>Called by : <USER>                                       *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
C.
C.    ------------------------------------------------------------------
C.
      NZEBRA=NZEB
      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/GCBANK/',' ',FENDQ,LQ,LR1,WS,LQ(KWWORK+100)
     +            ,LQ(NZEBRA-30))
      CALL MZLOGL(IXSTOR,0)
C
      RETURN
      END
