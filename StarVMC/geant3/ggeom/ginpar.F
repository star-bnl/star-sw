*
* $Id: ginpar.F,v 1.1.1.2 2009/02/01 17:10:25 fisyak Exp $
*
* $Log: ginpar.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:25  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:45  fisyak
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
* Revision 1.1.1.1  1995/10/24 10:20:50  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.29  by  S.Giani
*-- Author :
      SUBROUTINE GINPAR(X,PAR,IYES)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    Routine to determine whether a point X is inside the        *
C.    *    parallelepiped with parameters PAR. IYES is returned as 1   *
C.    *    if it is; as zero if it isn't.                              *
C.    *                                                                *
C.    *    ==>Called by : GINME                                        *
C.    *         Author  A.McPherson  *********                         *
C.    *                                                                *
C.    ******************************************************************
C.
      DIMENSION X(3),PAR(6)
C.
C.           -------------------------------------
C.
      IYES=0
      IF(ABS(X(3)).GT.PAR(3)) GO TO 99
C
      YT=X(2)-PAR(6)*X(3)
      IF(ABS(YT).GT.PAR(2)) GO TO 99
C
      XT=X(1)-PAR(5)*X(3)-PAR(4)*YT
      IF(ABS(XT).GT.PAR(1)) GO TO 99
C
      IYES=1
C
   99 CONTINUE
      END
