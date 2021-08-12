* $Id: gfvert.F,v 1.1.1.1 2004/01/15 00:12:12 potekhin Exp $
*
* $Log: gfvert.F,v $
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
C*    * PN, 20.11.03: A default size of UBUF(10) is introduced.        *
C*    *If more user words should to be read from KINE bank, user has to*
C*    * - allocate enouph memory in his local UBUF array;              *
C*    * - set the available maximum by calling GFKINE_SETMAX(NWBUF),   *
C*    *   where NWBUF is the actial dimension of the UBUF array;       *
C*    * - call GFKINE_RESET on exit (in order to prevent badly written *
C*    *   legacy codes from overwriting its memory).                   *
C*    * A negative NWBUF changes the default UBUF size to abs(NWBUF)   *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcnum.inc"
      DIMENSION V(3),UBUF(1)
      INTEGER   NWBMAX/10/,NWBMAX0/10/
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
      JV=LQ(JVERTX-NVTX)
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
            DO 20 IBUF=1,min(NWBUF,NWBMAX)
               UBUF(IBUF)=Q(JUV+IBUF)
   20       CONTINUE
         ENDIF
      ENDIF
*
10000 FORMAT(' **** GFVERT: No vertex stored in memory')
10100 FORMAT(' **** GFVERT: Vertex ',I10,' does not exist')
  999 RETURN
*
      ENTRY GFVERT_SETMAX(NWBUF)
      if (NWBUF.ge.0) NWBMAX = NWBUF
      if (NWBUF.lt.0) NWBMAX0=-NWBUF
      return
*    
      ENTRY GFVERT_GETMAX(NWBUF)
      NWBUF=NWBMAX
      return
*    
      ENTRY GFVERT_RESET
      NWBMAX=NWBMAX0
      return
      END
