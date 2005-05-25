*
* $Id: gckmax.inc,v 1.1.1.1 2005/05/25 22:37:31 fisyak Exp $
*
* $Log: gckmax.inc,v $
* Revision 1.1.1.1  2005/05/25 22:37:31  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCKMAX_INC
#define CERNLIB_GEANT321_GCKMAX_INC
*
*
* gckmax.inc
*
#if !defined(CERNLIB_INCLUDE)
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
#endif
*
* gckmax.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gckmax
#include "geant321/incend.inc"
#endif

#endif
