*
* $Id: gfvert.F,v 1.1 2000/01/06 22:09:15 nevski Exp $
*
* $Log: gfvert.F,v $
* Revision 1.1  2000/01/06 22:09:15  nevski
* unlimited track/vertex version
*
* Revision 1.2  1998/02/10 16:04:20  japost
*   Comments are correctly redistributed.
*
* Revision 1.1.1.1  1995/10/24 10:21:18  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GFVERT(NVTX,V,NTBEAM,NTTARG,TOFG,UBUF,NWBUF)
C.
C.    ******************************************************************
C.    *                                                                *
C     *       Retrieves the parameter of a vertex bank                 *
C     *       Vertex is generated from tracks NTBEAM NTTARG            *
C     *       NVTX is the new vertex number                            *
C.    *                                                                *
C.    *    ==>Called by : <USER>                                       *
C.    *       Authors    F.Carminati                   *********       *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcnum.inc"
      DIMENSION V(3),UBUF(1)
C.
C.    ------------------------------------------------------------------
C.
      IF (JVERTX.EQ.0.OR.NVERTX.LE.0)THEN
         WRITE(CHMAIL,10000)
         CALL GMAIL(0,0)
         GOTO 999
      ENDIF
      IF(NVTX.GT.NVERTX) THEN
         WRITE(CHMAIL,10100)NVERTX
         CALL GMAIL(0,0)
         GOTO 999
      ENDIF
      JV=LgVERT(JVERT,NVTX)
      IF(JV.LE.0) THEN
         WRITE(CHMAIL,10100)NVERTX
         CALL GMAIL(0,0)
         GOTO 999
      ENDIF
      DO 10 I=1,3
         V(I) = Q(JV + I)
   10 CONTINUE
      TOFG = Q(JV + 4)
      NTBEAM = Q(JV + 5)
      NTTARG = Q(JV + 6)
*
      NWBUF=0
      IF(IQ(JV-2).GE.1) THEN
         JUV=LQ(JV-1)
         IF(JUV.GT.0) THEN
            NWBUF=IQ(JUV-1)
            DO 20 IBUF=1,NWBUF
               UBUF(IBUF)=Q(JUV+IBUF)
   20       CONTINUE
         ENDIF
      ENDIF
*
10000 FORMAT(' **** GFVERT: No vertex stored in memory')
10100 FORMAT(' **** GFVERT: Vertex ',I10,' does not exist')
  999 END
