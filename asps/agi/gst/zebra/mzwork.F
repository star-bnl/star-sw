*
* $Id: mzwork.F,v 1.4 1998/06/05 20:55:40 perev Exp $
*
* $Log: mzwork.F,v $
* Revision 1.4  1998/06/05 20:55:40  perev
* AGI commit
*
* Revision 1.3  1998/04/16 16:58:09  fisyak
* 2nd pass with gstar
*
*
#include "zebra/pilot.h"
*CMZ :          06/12/97  11.43.18  by  Pavel Nevski
*-- Author :
      SUBROUTINE MZWORK (IXSTOR,DFIRST,DLAST,IFLAGP)
 
C-    Allocate working space, user called
 
C-    IFLAG = -1  reset wsp empty
C-             0  new wsp
C-             1  vary both limits, keep common links
C-             2  vary  only DLAST  limit, keep links and common data
C-             3  reset only DFIRST limit, zero all links
C-             4  vary  only DFIRST limit, keep common links
C-            >5  allocate reserve between WS and DIV1
#include "zebra/zmach.inc"
#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mqsys.inc"
      COMMON /MZCDN/ LQRSTA(20),LQREND(20)
C--------------    End CDE                             --------------
      INTEGER      DFIRST(9), DLAST(9), IFLAGP(9)
#if (defined(CERNLIB_QTRHOLL))&&(!defined(CERNLIB_A6M))
      DIMENSION    NAMESR(2)
      DATA  NAMESR / 4HMZWO, 4HRK   /
#endif
#if (defined(CERNLIB_QTRHOLL))&&(defined(CERNLIB_A6M))
      DATA  NAMESR / 6HMZWORK /
#endif
#if !defined(CERNLIB_QTRHOLL)
      CHARACTER    NAMESR*8
      PARAMETER   (NAMESR = 'MZWORK  ')
#endif
 
#include "zebra/q_jbit.inc"
#include "zebra/q_locf.inc"
#include "zebra/qtrace.inc"
#include "zebra/qstore.inc"
 
      IFLAG = IFLAGP(1)
      JSTO  = JBYT (IXSTOR,27,4)
      IF (JSTO.GT.NQSTOR) GO TO 91
 
C----              Reset empty working space
 
      NEWL  = NQREF
      NEWD  = NQREF
      IF (IFLAG.LT.0)              GO TO 71
      NEWL  = LOCF (DFIRST(1)) - (LQSTOR+1)
      NEWD  = LOCF (DLAST(1))  -  LQSTOR
 
      IF (IFLAG.GE.5)              then
         LQRSTA(JSTO+1)=NEWL
         LQREND(JSTO+1)=NEWD
         GO TO 999
      endif
 
      IF (IFLAG.GE.3)  NEWD = LQSTA(1) - 1
      IF (IFLAG.EQ.2)  NEWL = NQLINK
 
C----              Check valid parameters
 
      IF (NEWL.LT.NQREF)           GO TO 92
      IF (NEWD.LT.NEWL)            GO TO 93
      IF (IFLAG.GE.3)              GO TO 31
      IF (LQRSTA(JSTO+1).EQ.0) then
         IF (NEWD.GE.LQEND(KQT+2)) GO TO 94
      ELSE
         IF (NEWD.GE.LQRSTA(JSTO+1)) GO TO 94
      ENDIF
 
C--                Check garbage collection
 
      NQRESV = LQSTA(KQT+2) - NQMINR - NEWD
      IF (NQRESV.GT.0)             GO TO 31
      print *,' negative NQRESV force garbage collection = ',NQRESV
      LQEND(KQT+1) = LQSTA(KQT+1)
      JQDIVI = 2
      CALL MZRESV
      CALL MZGAR1
      NQRESV = LQSTA(KQT+2) - NQMINR - NEWD
      IF (NQRESV.LE.0)             GO TO 94
 
C--                Clear new links
 
   31 LA = NQREF
      IF (IFLAG.EQ.1)  LA = NQLINK
      IF (IFLAG.EQ.2)  LA = NQLINK
      IF (IFLAG.EQ.4)  LA = NQLINK
      N  = NEWL - LA
      IF (N.GT.0)  CALL VZERO (LQ(KQS+LA+1),N)
 
C----              Set new limits
 
   71 NQLINK  = NEWL
      IQTABV(KQT+5) = NQLINK
      IF (IFLAG.LT.3)  THEN
          IF (NEWD.GE.LQSTA(KQT+1)) Then
             LQSTA(KQT+1) = LQREND(JSTO+1)
             IF (LQSTA(KQT+1).EQ.0)  LQSTA(KQT+1) = NEWD + 1
             LQEND(KQT+1) = LQSTA(KQT+1)
          endif
        ENDIF
 
      IF (NQLOGL.GE.2) WRITE (IQLOG,9071) JQSTOR,IFLAG,NQLINK,NEWD
 9071 FORMAT (' MZWORK-  Store',I3,' Flag=',I2,' Last Link/Data',2I7)
 
#include "zebra/qtrace99.inc"
      RETURN
 
C------            Error conditions
 
   94 NQCASE = 1
   93 NQCASE = NQCASE + 1
   92 NQCASE = NQCASE + 1
   91 NQCASE = NQCASE + 1
      NQFATA = 4
      IQUEST(11) = NQREF
      IQUEST(12) = NEWL
      IQUEST(13) = NEWD
      IQUEST(14) = IFLAG
#include "zebra/qtofatal.inc"
      END
*      ==================================================
#include "zebra/qcardl.inc"
