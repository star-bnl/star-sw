*
* $Id: gpvert.F,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
*
* $Log: gpvert.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.2  2003/08/12 12:50:07  nevski
* improved output format
*
* Revision 1.1  2001/02/27 10:15:40  nevski
*  first working release
*
* Revision 1.2  2000/01/06 22:09:16  nevski
* unlimited track/vertex version
*
* Revision 1.1  1998/12/28 15:06:55  nevski
* minor format corrections
*
* Revision 1.1.1.1  1995/10/24 10:21:18  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GPVERT(IV)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Prints vertex bank JVERTX for vertex number IV           *
C.    *       If IV=0 Prints all vertex banks                          *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GPRINT                               *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.    Modifications
C.    PN, 23/12/98  format 2000 corrected
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gcunit.inc"
C.
C.    ------------------------------------------------------------------
C.
      WRITE(CHMAIL,1000)
      CALL GMAIL(1,0)
C
      I1=1
      I2=NVERTX
      IF(IV.NE.0)I1=IV
      IF(IV.NE.0)I2=IV
      IF(I2.GT.NVERTX)GO TO 99
      IF(I1.LT.1)GO TO 99
      IF(JVERTX.LE.0)GO TO 99
C
      DO 20 IVERT=I1,I2
         JV=LgVERT(JVERT,IVERT)
         IF(JV.NE.0)THEN
            TOFGN=Q(JV+4)*1.E+9
            NL=IQ(JV-2)
            NG=Q(JV+7)+7
            NW=MIN(NG,16)
            WRITE(CHMAIL,2000)IVERT,(Q(JV+I),I=1,3),TOFGN,
     +          INT(Q(JV+5)),INT(Q(JV+6)),(INT(Q(JV+L)),L=8,NW)
  10        CALL GMAIL(0,0)
            IF(NG-NW.GT.0) THEN
               NS=NW+1
               NW=MIN(NG,NW+9)
               WRITE(CHMAIL,3000)(INT(Q(JV+L)),L=NS,NW)
               GOTO 10
            ENDIF
            IF(NL.GT.0) THEN
               JUV = LQ(JV-1)
               IF(JUV.GT.0) THEN
                  WRITE(CHMAIL,4000)
                  CALL GMAIL(0,0)
                  ND  = IQ(JUV-1)
                  NW=MIN(ND,9)
                  WRITE(CHMAIL,5000) (Q(JUV+L),L=1,NW)
   15             CALL GMAIL(0,0)
                  IF(ND-NW.GT.0) THEN
                     NS=NW+1
                     NW=MIN(ND,NW+9)
                     WRITE(CHMAIL,5000) (Q(JUV+L),L=NS,NW)
                     GO TO 15
                  END IF
               END IF
            END IF
         ENDIF
   20 CONTINUE
C
 1000 FORMAT(3X,'Vert',8X,'Vx',10X,'Vy',10X,'Vz',7X,'Tofgn',
     +       3X,'Tbeam',4X,'Targ',14X,'Gener tracks')
 2000 FORMAT(I7,3F12.4,F11.4,I7,I8,1X,9I6)
 3000 FORMAT(70X,9I6)
 4000 FORMAT(62X,'User words')
 5000 FORMAT(1X,9E14.5)
C
  99  RETURN
      END

