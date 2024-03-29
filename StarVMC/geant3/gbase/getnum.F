*
* $Id: getnum.F,v 1.1.1.2 2009/02/01 17:10:18 fisyak Exp $
*
* $Log: getnum.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:18  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:46:57  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:25  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:24  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:37  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.19  by  S.Giani
*-- Author :
      SUBROUTINE GETNUM(LIST,N)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to count the number of non blank elements        *
C.    *      in the array LIST before the first blank one              *
C.    *                                                                *
C.    *    ==>Called by : GFFGO                                        *
C.    *    ==>Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
      DIMENSION LIST(*)
      SAVE IFIRST,IDFLT
      DATA IFIRST/0/
C.    ------------------------------------------------------------------
C.
      IF(IFIRST.EQ.0)THEN
         IFIRST=1
         CALL UCTOH('    ',IDFLT,4,4)
      ENDIF
C
      N = 0
      DO 10 I=1,20
         IF(LIST(I).EQ.IDFLT) GO TO 99
         N=N+1
  10  CONTINUE
C
  99  RETURN
      END
