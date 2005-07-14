*
* $Id: guxcs.F,v 1.1.1.1 2005/05/25 22:37:57 fisyak Exp $
*
* $Log: guxcs.F,v $
* Revision 1.1.1.1  2005/05/25 22:37:57  fisyak
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
* Revision 1.1.1.1  1995/10/24 10:21:47  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/04 06/12/94  16.09.54  by  S.Ravndal
*-- Author :    S.Ravndal   06/12/94
      SUBROUTINE GUXCS
*
*              User routine  to declare addresses of FORTRAN routines
*              and COMMONs  which may be invoked from COMIS routines.
*              Called by GXCS
*
#include "geant321/gcomis.inc"
*
      DIMENSION P(1)
*
*
      END
