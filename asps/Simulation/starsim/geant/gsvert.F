*
* $Id: gsvert.F,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
* $Log: gsvert.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.1  2001/02/27 10:15:42  nevski
*  first working release
*
* Revision 1.4  2000/01/07 00:48:48  nevski
* unlimited track/vertex version
*
#include "geant321/pilot.h"
*CMZ :  1.30/00 03/05/97  16.32.36  by  Pavel Nevski
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GSVERT(V,NTBEAM,NTTARG,UBUF,NWBUF,NVTX)
C.
C.    ******************************************************************
C.    *                                                                *
C     *       Creates a new vertex bank                                *
C     *       Vertex is generated from tracks NTBEAM NTTARG            *
C     *       NVTX is the new vertex number                            *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GUKINE,GUSTEP                        *
C.    *       Authors    R.Brun, F.Carena, M.Hansroul  *********       *
C.    *                                                                *
C.    * Modifications:                                                 *
C.    * PN, 07/Feb/97: allow negative NTbeam and NTtarg                *
C.    ******************************************************************
C.
#include "commons/typing.inc"
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcpush.inc"
#include "geant321/gctrak.inc"
#include "geant321/gckine.inc"
#include "geant321/gcunit.inc"
#include "commons/quest.inc"

      REAL     V(3),UBUF(1)
      INTEGER  LGKINE,LGVERT,NTBEAM,NTTARG,NWBUF,NVTX,NT(2),IER,
     >         JV,JUV,I,J,JK,NVG,NVI,NFREE,JVRT,NPUSH
C.
C.    ------------------------------------------------------------------
C.
      CALL AGNEED (IER)
      IF (IER.NE.0) GO TO 90

      NVTX   = NVERTX + 1
      IF (JVERTX.EQ.0)THEN
C        simulated vertex list
         CALL MZBOOK(IXDIV,JVERTX,JVERTX,1,'VERT',NCVERT,NCVERT,2,2,0)
         IQ(JVERTX-5)=1
      ENDIF
C-
      J   = LgVERT(JVRT,NVTX)
      NVI = NVTX-IQ(JVRT+2)
*
      IF (NVI.GT.64000) then
         CALL MZBOOK(IXDIV,JVRT,JVRT,0,'VERT',NCVERT,NCVERT,2,2,0)
         IQ(JVRT+2)=NVERTX
         J   = LgVERT(JVRT,NVTX)
         NVI = NVTX-IQ(JVRT+2)
      endif

      IF (NVI.GT.IQ(JVRT-2)) then
         NPUSH=min(NPVERT*int(Alog(NVTX+2.)),64000-IQ(JVRT-2))
         CALL MZPUSH(IXDIV,JVRT,NPUSH,0,'I')
      ENDIF
C-
      IF (NVI.GT.IQ(JVRT-2)) THEN
         PRINT *,' GSVERT ERROR: WRONG NVertex ',NVTX
         GOTO 90
      ENDIF

C     one vertex parameters
      NVERTX    = NVTX
      IQ(JVRT+1)= NVERTX
      CALL MZBOOK(IXDIV,JV,JVRT,-NVI,'VERT',1,1,9,3,0)
      IQ(JV-5)  = NVTX
      DO 4 I=1,3
   4  Q(JV + I) = V(I)
      Q(JV + 4) = TOFG
      Q(JV + 5) = NTBEAM
      Q(JV + 6) = NTTARG

C     add users buffer
      IF (NWBUF.GT.0) THEN
         CALL MZBOOK(IXDIV,JUV,JV,-1,'VERU',0,0,NWBUF,2,0)
         IQ(JUV-5)=NVTX
         DO 3 I=1,NWBUF
   3     Q(JUV+I)=UBUF(I)
      ENDIF
C
      NT(1) = NTBEAM
      NT(2) = NTTARG
      Do i=1,2
         IF (NT(i).GT.0) THEN
            JK   = LgKINE(J,NT(i))
            IF(JK.LE.0) GO TO 90
            NVG  = Q(JK + 7)
            NFREE=IQ(JK-1)-7-NVG
            IF(NFREE.LE.0) CALL MZPUSH(IXDIV,JK,0,max(2,NVG/5),'I')
            Q(JK + NVG + 8) = NVTX
            Q(JK + 7) = NVG + 1
         ENDIF
      EndDO
      RETURN
C
C             Error
C
  90  NVTX   = 0
      END

