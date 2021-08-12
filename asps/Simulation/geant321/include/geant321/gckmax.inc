*
* $Id: gckmax.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gckmax.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.2  2003/11/16 17:41:04  nevski
* follow cernlib 2002 release note
*
* Revision 1.1.1.1  1997/11/03 15:29:48  atlascvs
* Importing CERNLIB version 08.21.
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
      PARAMETER (MXGKIN=250)
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
