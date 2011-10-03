*
* $Id: lujets.inc,v 1.1.1.1 2004/01/15 00:12:29 potekhin Exp $
*
* $Log: lujets.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:29  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:52  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:34  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_LUJETS_INC
#define CERNLIB_GEANT321_LUJETS_INC
*
*
* lujets.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      INTEGER N,K
      REAL P,V
      SAVE /LUJETS/
*
#endif
*
* lujets.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +    lujets
#include "geant321/incend.inc"
#endif

#endif
