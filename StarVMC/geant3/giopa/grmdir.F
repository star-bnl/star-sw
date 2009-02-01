*
* $Id: grmdir.F,v 1.1.1.2 2009/02/01 17:10:30 fisyak Exp $
*
* $Log: grmdir.F,v $
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
      SUBROUTINE GRMDIR(CHDIR,CHOPT)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to create a "standard" GEANT subdirectory        *
C.    *                                                                *
C.    *       CHDIR Subdirectory name                                  *
C.    *                                                                *
C.    *       CHOPT is a character string which may be                 *
C.    *          ' ' To create a subdirectory                          *
C.    *          'S' To create a subdirectory and set the new          *
C.    *              Current Directory to this directory.              *
C.    *                                                                *
C.    *    ==>Called by : <USER>                                       *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
      CHARACTER*(*) CHDIR,CHOPT
      CHARACTER*8 CHTAGS(2)
      DATA CHTAGS/'Object','Version'/
C.
C.    ------------------------------------------------------------------
C.
      CALL UOPTC(CHOPT,'S',IOPTS)
*
      CALL RZMDIR(CHDIR,2,'HI',CHTAGS)
      IF(IOPTS.NE.0)THEN
         CALL RZCDIR(CHDIR,' ')
      ENDIF
*
      END
