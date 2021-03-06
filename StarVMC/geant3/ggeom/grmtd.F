*
* $Id: grmtd.F,v 1.1.1.2 2009/02/01 17:10:26 fisyak Exp $
*
* $Log: grmtd.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:26  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:55  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:36  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:25  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:39  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:17  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:54  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.30  by  S.Giani
*-- Author :
      SUBROUTINE GRMTD(XMAS,IROT,XNEW)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    ROUTINE ROTATES A POINT IN XMAS FROM THE MASTER SYSTEM      *
C.    *    TO A NEW SYSTEM USING MATRIX # IROT.                        *
C.    *    RESULT IS RETURNED IN VECTOR XNEW.                          *
C.    *                                                                *
C.    *    ==>Called by : GNEXT, GTNEXT                                *
C.    *         Author  A.McPherson  *********                         *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
      DIMENSION XMAS(3),XNEW(3)
C.
C.    ------------------------------------------------------------------
C.
      IF (IROT.GT.0) THEN
         JR = LQ(JROTM- IROT)
         XNEW(1) = XMAS(1)*Q(JR+1) + XMAS(2)*Q(JR+2) + XMAS(3)*Q(JR+3)
         XNEW(2) = XMAS(1)*Q(JR+4) + XMAS(2)*Q(JR+5) + XMAS(3)*Q(JR+6)
         XNEW(3) = XMAS(1)*Q(JR+7) + XMAS(2)*Q(JR+8) + XMAS(3)*Q(JR+9)
*
      ELSE
         XNEW(1) = XMAS(1)
         XNEW(2) = XMAS(2)
         XNEW(3) = XMAS(3)
*
      ENDIF
      END
