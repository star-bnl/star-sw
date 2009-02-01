*
* $Id: gevkev.F,v 1.3 2009/02/01 17:29:50 fisyak Exp $
*
* $Log: gevkev.F,v $
* Revision 1.3  2009/02/01 17:29:50  fisyak
* Resolve conflicts
*
* Revision 1.1.1.1  2008/12/10 20:47:00  fisyak
* Merge with macos version
*
* Revision 1.2  2005/05/26 22:41:35  fisyak
* recover gevkev from g3evkev (used in egr)
*
* Revision 1.1.1.1  2005/05/25 22:37:26  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:55  brun
* New version of geant321 with all geant3 routines renamed from G to G3
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
* Revision 1.1.1.1  1995/10/24 10:20:13  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.19  by  S.Giani
*-- Author :
      SUBROUTINE GEVKEV(EGEV,ENERU,KUNIT)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Convert energy from GeV to more appropriate units        *
C.    *                                                                *
C.    *    ==>Called by : G3PLMAT,G3PRMAT,G3PHYSI,G3PCXYZ              *
C.    *       Author     M.Maire    *********                          *
C.    *                                                                *
C.    ******************************************************************
C.
      CHARACTER*4 KUNIT
*
      IF(EGEV.LT.0.0000009999) THEN
         ENERU = EGEV*1.E+9
         KUNIT = '  eV'
      ELSEIF(EGEV.LT.0.0009999) THEN
         ENERU = EGEV*1.E+6
         KUNIT = ' keV'
      ELSEIF(EGEV.LT.0.9999) THEN
         ENERU = EGEV*1.E+3
         KUNIT = ' MeV'
      ELSEIF(EGEV.LT.999.9) THEN
         ENERU = EGEV
         KUNIT = ' GeV'
      ELSE
         ENERU = EGEV*1.E-3
         KUNIT = ' TeV'
      ENDIF
*
      END
      SUBROUTINE G3EVKEV(EGEV,ENERU,KUNIT)
      CHARACTER*4 KUNIT
      call GEVKEV(EGEV,ENERU,KUNIT)
      end
