*
* $Id: gfndig.F,v 1.1.1.2 2009/02/01 17:10:28 fisyak Exp $
*
* $Log: gfndig.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:28  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:18  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:37  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:25  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:40  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:19  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.20  by  S.Giani
*-- Author :
      SUBROUTINE GFNDIG (IUSET, IUDET, NDIGI)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GFNDIG (IUSET, IUDET, NDIGI*)                         *
C.    *                                                                *
C.    *   Returns the number of digits belonging to the detector IUDET *
C.    *   of set IUSET                                                 *
C.    *                                                                *
C.    *   IUSET    User set identifier                                 *
C.    *   IUDET    User detector identifier (name of the corresponding *
C.    *            sensivitive volume)                                 *
C.    *   NDIGI    returns the total number of digits                  *
C.    *                                                                *
C.    *   Called by : <USER>                                           *
C.    *   Author    : S.Banerjee                                       *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
      CHARACTER*4     IUSET, IUDET
C.
C.    ------------------------------------------------------------------
*
* *** Find if selected set, detector exists
*
      NDIGI = 0
      IF (JDIGI.LE.0)   GO TO 999
      NSET  = IQ(JSET-1)
      CALL GLOOK (IUSET, IQ(JSET+1), NSET, ISET)
      IF (ISET.LE.0)    GO TO 999
*
      JS    = LQ(JSET-ISET)
      JDI   = LQ(JDIGI-ISET)
      IF (JS.LE.0)      GO TO 999
      IF (JDI.LE.0)     GO TO 999
      NDET  = IQ(JS-1)
      CALL GLOOK (IUDET, IQ(JS+1), NDET, IDET)
      IF (IDET.LE.0)    GO TO 999
*
      JDID  = LQ(JDI-IDET)
      IF (JDID.LE.0)    GO TO 999
      ILAST = IQ(JDI+IDET)
*
      IF (ILAST.NE.0) NDIGI = IQ(JDID+ILAST)
*                                                             END GFNDIG
  999 END
