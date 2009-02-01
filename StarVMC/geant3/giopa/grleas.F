*
* $Id: grleas.F,v 1.1.1.2 2009/02/01 17:10:30 fisyak Exp $
*
* $Log: grleas.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:30  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:27  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:53  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:25  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:40  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:20  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:17  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GRLEAS(JBANK)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Release unused space in data structure pointed by JBANK. *
C.    *       JBANK can be either JHITS or JDIGI                       *
C.    *                                                                *
C.    *    ==>Called by : GSAVE                                        *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
C.
C.    ------------------------------------------------------------------
C.
      IF(JBANK.LE.0)GO TO 99
      NS=IQ(JBANK-2)
      IF(NS.LE.0)GO TO 99
      DO 20 IS=1,NS
         JB=LQ(JBANK-IS)
         IF(JB.GT.0)THEN
            ND=IQ(JB-1)
            IF(ND.GT.0)THEN
               DO 10 ID=1,ND
                  JBD=LQ(JB-ID)
                  IF(JBD.GT.0)THEN
                     NPUSH=IQ(JBD-1)-IQ(JB+ID)
                     IF(NPUSH.GT.0)CALL MZPUSH(IXDIV,JBD,0,-NPUSH,'I')
                  ENDIF
  10           CONTINUE
            ENDIF
         ENDIF
  20  CONTINUE
C
  99  RETURN
      END
