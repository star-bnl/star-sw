* $Id: gfkine.F,v 1.1.1.1 2004/01/15 00:12:12 potekhin Exp $
*
* $Log: gfkine.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:12  potekhin
*
* Revision 1.4  2003/11/25 11:59:36  nevski
* bugfix - min instead of max
*
* Revision 1.3  2003/11/21 17:35:10  nevski
* make user buffer size controllable from outside
*
* Revision 1.1.1.1  1997/11/03 15:30:29  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:18  cernlib
* Geant
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GFKINE(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NWBUF)
*
************************************************************************
*                                                                      *
*           Storing/Retrieving Vertex and Track parameters             *
*           ----------------------------------------------             *
*                                                                      *
*  Stores vertex parameters.                                           *
*  VERT      array of (x,y,z) position of the vertex                   *
*  NTBEAM    beam track number origin of the vertex                    *
*            =0 if none exists                                         *
*  NTTARG    target track number origin of the vertex                  *
*  UBUF      user array of NUBUF floating point numbers                *
*  NUBUF                                                               *
*  NVTX      new vertex number (=0 in case of error).                  *
*  Prints vertex parameters.                                           *
*  IVTX      for vertex IVTX.                                          *
*            (For all vertices if IVTX=0)                              *
*  Stores long life track parameters.                                  *
*  PLAB      components of momentum                                    *
*  IPART     type of particle (see GSPART)                             *
*  NV        vertex number origin of track                             *
*  UBUF      array of NUBUF floating point user parameters             *
*  NUBUF                                                               *
*  NT        track number (if=0 error).                                *
*  Retrieves long life track parameters.                               *
*  ITRA      track number for which parameters are requested           *
*  VERT      vector origin of the track                                *
*  PVERT     4 momentum components at the track origin                 *
*  IPART     particle type (=0 if track ITRA does not exist)           *
*  NVERT     vertex number origin of the track                         *
*  UBUF      user words stored in GSKINE.                              *
*  Prints initial track parameters.                                    *
*  ITRA      for track ITRA                                            *
*            (For all tracks if ITRA=0)                                *
*                                                                      *
*    ==>Called by : <USER>, GTREVE                                     *
*       Author    R.Brun  *********                                    *
*                                                                      *
*  PN, 20.11.03: A default size of UBUF(10) is introduced.             *
*    If more user words should to be read from KINE bank, user has to: *
*      - allocate enouph memory in his local UBUF array;               *
*      - set the available maximum by calling GFKINE_SETMAX(NWBUF),    *
*        where NWBUF is the actial dimension of the UBUF array;        *
*      - call GFKINE_RESET on exit (in order to prevent badly written  *
*        legacy codes from overwriting its memory).                    *
*    A negative NWBUF changes only the default UBUF size to abs(NWBUF) *
************************************************************************
*
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gctrak.inc"
      DIMENSION VERT(3),PVERT(4),UBUF(*)
      INTEGER   NWBMAX/10/,NWBMAX0/10/
C.
C.    ------------------------------------------------------------------
C.
      IPART  = 0
      IF (JKINE.LE.0)GO TO 99
      IF (ITRA.LE.0)GO TO 99
      IF (ITRA.GT.NTRACK)GO TO 99
C
      JK = LQ(JKINE- ITRA)
      IF (JK.LE.0)GO TO 99
      NV = Q(JK + 6)
      IF (JVERTX.LE.0)GO TO 99
      JV = LQ(JVERTX- NV)
      IF (JV.LE.0)GO TO 99
      DO 10 I=1,3
  10  VERT(I) = Q(JV + I)
*     TOFG=Q(JV+4)
C
      DO 20 I=1,4
  20  PVERT(I) = Q(JK + I)
      IPART = Q(JK + 5)
      NVERT = Q(JK + 6)
      NLINK = IQ(JK-2)
      NWBUF  = 0
      IF (NLINK.LE.0)GO TO 99
      JKU = LQ(JK- 1)
      IF (JKU.LE.0)GO TO 99
      NWBUF = IQ(JKU-1)
      DO 30 I=1,min(NWBUF,NBFMAX)
  30  UBUF(I)=Q(JKU+I)
C
  99  RETURN
*    
      ENTRY GFKINE_SETMAX(NWBUF)
      if (NWBUF.ge.0) NWBMAX = NWBUF
      if (NWBUF.lt.0) NWBMAX0=-NWBUF
      return
*    
      ENTRY GFKINE_GETMAX(NWBUF)
      NWBUF=NWBMAX
      return
*
      ENTRY GFKINE_RESET
      NWBMAX=NWBMAX0
      return
      END
