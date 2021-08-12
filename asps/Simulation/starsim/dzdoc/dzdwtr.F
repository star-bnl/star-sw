*
* $Id: dzdwtr.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: dzdwtr.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.3  2001/06/15 00:02:18  nevski
* merging differences between private cmz and this version
*
* Revision 1.2  2001/03/25 17:50:32  nevski
* mzca splitted
*
* Revision 1.1  2001/02/27 10:15:23  nevski
*  first working release
*
#include "dzdoc/pilot.h"
*
**************************************************************************
*                                                                        *
       SUBROUTINE DZDWTR(ISTORE,L,LRET)
*                                                                        *
*     walk through a d/s at L, return link to the next bank in LRET      *
*     Author :    Otto Schaile   14/02/96                                *
**************************************************************************
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
      INTEGER L,LIN,LRET,JB
      INTEGER LORIG, LUP, LFROM
      SAVE    LORIG, LUP, LFROM
*---
      CALL MZSDIV(ISTORE,-7)
*     init links and return
      IF(L.LT.0)THEN
         LRET=-L
         LORIG = LRET
         LUP = LRET
         LFROM = LQ(KQS+LRET+1)
         GOTO 90
      ENDIF
*
      LIN=L
      JB = LUP - LORIG + 1
   10 CONTINUE
      NS = IQ(KQS+LUP -2)
      IF(JB .LE. NS)THEN
*        go down
         LD1 = LQ(KQS+ LIN - JB)
*        look if this link is used
         IF(LD1 .EQ. 0)THEN
            JB = JB + 1
            GOTO 10
         ENDIF
         LUP = LD1
         LORIG = LD1
         LRET = LD1
         GOTO 90
      ENDIF

*     look if its part of linear structure

      LN = LQ(KQS+LIN)
      IF(LN .NE. 0)THEN
         LUP = LN
         LORIG = LN
         LRET = LN
         GOTO 90
      ENDIF

*     look if it is end of a linear structure
*     i.e. origin and up link are in different banks

   50 CONTINUE
      LUP = LQ(KQS+LIN+1)
      LORIG = LQ(KQS+LIN+2)
*     look if back at top bank or if it would move above top bank
      IF(LUP .LE. 1 .OR. LUP .EQ. LFROM) THEN
         LRET=0
         GOTO 99
      ENDIF
      NSU = IQ(KQS+LUP-2)
      IF(LORIG .GE. LUP .OR. LORIG .LT. LUP-NSU)THEN
*        its different, step back
         LIN = LORIG
         GOTO 50
      ENDIF
*     go up
      LRET = LUP
   90 CONTINUE
      JB = LUP - LORIG + 1
      IF(JB.NE.1) THEN
         LIN=LRET
         GOTO 10
      ENDIF
   99 CONTINUE
      END
