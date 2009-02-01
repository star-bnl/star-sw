*
* $Id: grend.F,v 1.1.1.2 2009/02/01 17:10:30 fisyak Exp $
*
* $Log: grend.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:30  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:26  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:38  fisyak
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
*CMZ :  3.21/02 29/03/94  15.41.20  by  S.Giani
*-- Author :
      SUBROUTINE GREND (LUN)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to close an RZ file                              *
C.    *                                                                *
C.    *       CHDIR    Top directory name of the file                  *
C.    *                                                                *
C.    *    ==>Called by : <USER>                                       *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
      CHARACTER*5 CHDIR
C.
C.    ------------------------------------------------------------------
C.
      IF(LUN.LT.10) THEN
         WRITE(CHDIR,'(''LUN'',I1)') LUN
      ELSE
         WRITE(CHDIR,'(''LUN'',I2)') LUN
      ENDIF
      CALL RZEND(CHDIR)
C
      END
