*
* $Id: gfkine.F,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
*
* $Log: gfkine.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.3  2003/11/21 17:25:13  nevski
* make user buffer size controllable from outside
*
* Revision 1.2  2003/11/17 19:52:43  nevski
* put protection for UBUF shortage
*
* Revision 1.1  1998/08/28 14:46:49  nevski
* interface KINE bank via LgKINE to eliminate 64k limitation
*
#include "geant321/pilot.h"
*CMZ :          28/08/98  10.37.39  by  Pavel Nevski
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GFKINE(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NWBUF)
*
************************************************************************
*                                                                      *
*           Retrieves Vertex and Track parameters                      *
*           -------------------------------------                      *
*                                                                      *
*  Retrieves parameters for a track with a long lifetime.              *
*                                                                      *
*  Input:                                                              *
*    ITRA      track number for which parameters are requested         *
*                                                                      *
*  Output:                                                             *
*    VERT      vector origin of the track                              *
*    PVERT     4 momentum components at the track origin               *
*    IPART     particle type (=0 if track ITRA does not exist)         *
*    NVERT     vertex number origin of the track                       *
*    UBUF      user words stored in GSKINE.                            *
*                                                                      *
*    ==>Called by : <USER>, GTREVE                                     *
*       Author    R.Brun  *********                                    *
*  modifications:                                                      *
*  PN, 28.08.98:  interface to kine bank via LgKINE                    *
*  PN, 20.11.03:  default size of the UBUF(10) is introduced.          *
*    If more user words should to be read from KINE bank, user has to: *
*      - allocate enouph memory in his local UBUF array;               *
*      - set the available maximum by calling GFKINE_SETMAX(NWBUF),    *
*        where NWBUF is the actial dimension of the UBUF array;        *
*      - call GFKINE_RESET on exit (in order to prevent badly written  *
*        legacy codes from memory overwrite).                          *
*    A negative NWBUF changes only the default UBUF size to abs(NWBUF) *
************************************************************************
*
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gctrak.inc"
      DIMENSION VERT(3),PVERT(4),UBUF(*)
      INTEGER   NWBMAX/10./,NWBMAX0/10./
C.
C.    ------------------------------------------------------------------
C.
      IPART  = 0
      IF (JKINE.LE.0)GO TO 99
      IF (ITRA.LE.0)GO TO 99
      IF (ITRA.GT.NTRACK)GO TO 99
C
      JK = LgKINE(JKIN,ITRA)
      IF (JK.LE.0)GO TO 99
      NV = Q(JK + 6)
      IF (JVERTX.LE.0)GO TO 99
      JV = LgVERT(JVERT,NV)
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
      DO 30 I=1,min(NWBUF,NWBMAX)
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
