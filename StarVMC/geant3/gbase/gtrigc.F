*
* $Id: gtrigc.F,v 1.1.1.2 2009/02/01 17:10:19 fisyak Exp $
*
* $Log: gtrigc.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:19  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:00  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:25  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:55  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:24  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:37  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:11  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.19  by  S.Giani
*-- Author :
      SUBROUTINE G3TRIGC
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Clear event partition                                    *
C.    *                                                                *
C.    *    ==>Called by : GRUN   ,<GXINT> GINC4                        *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcstak.inc"
#include "geant321/gcflag.inc"
C.
C.    ------------------------------------------------------------------
C.
*
      IF (NJTMAX.LT.0) NJTMAX = -NJTMAX
*
      CALL GRNDMQ(IQ(JRUNG+19),IQ(JRUNG+20),0,'G')
      IQ(JRUNG+29) = IEVENT
*
      IF (JSTAK.NE.0) THEN
         IQ(JSTAK+1) = 0
         IQ(JSTAK+3) = 0
      ENDIF
*
      CALL MZWIPE (IXDIV)
*                                                             END GTRIGC
      END
 
