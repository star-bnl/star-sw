*
* $Id: gunear.F,v 1.1.1.1 2005/05/25 22:37:56 fisyak Exp $
*
* $Log: gunear.F,v $
* Revision 1.1.1.1  2005/05/25 22:37:56  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:42  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:21  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:46  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.25  by  S.Giani
*-- Author :
      SUBROUTINE GUNEAR(ISEARC, ICALL, X, JNEAR)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    User search                                                 *
C.    *       ISEARC to identify the given volume                      *
C.    *       ICALL  to identify the calling routine                   *
C.    *              1 GMEDIA like                                     *
C.    *              2 GNEXT like                                      *
C.    *       X      coordinates (+direction for ICALL=2)              *
C.    *       JNEAR  address of default list of neighbours             *
C.    *              (list to be overwriten by user)                   *
C.    *                                                                *
C.    *    Called by : GFTRAC, GINVOL, GTMEDI, GTNEXT, GNEXT, GMEDIA   *
C.    *                                                                *
C.    ******************************************************************
C.
      DIMENSION X(*)
C.    ------------------------------------------------------------------
*                                                             END GUNEAR
      END
