*
* $Id: uglast.F,v 1.1.1.1 2005/05/25 22:37:57 fisyak Exp $
*
* $Log: uglast.F,v $
* Revision 1.1.1.1  2005/05/25 22:37:57  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:57  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:42  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:21  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:52  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UGLAST
*
************************************************************************
*                                                                      *
*            Termination routine to print histograms and statistics    *
*                                                                      *
************************************************************************
#include "geant321/gcomis.inc"
*
*     -----------------------------------------------------------------
*
#if defined(CERNLIB_COMIS)
      IF(JULAST.NE.0)THEN
         CALL CSJCAL(JULAST,0,X,X,X,X,X,X,X,X,X,X)
         GO TO 99
      ENDIF
#endif
      CALL G3LAST
*
*             Close HIGZ
*
      CALL IGEND
*
  99  END
 
