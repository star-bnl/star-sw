*
* $Id: gcphpr.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcphpr.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:48  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:35  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCPHPR_INC
#define CERNLIB_GEANT321_GCPHPR_INC
*
*
* gcphpr.inc
*
#if !defined(CERNLIB_INCLUDE)
C  Probability of radiative decay mode.
      REAL GFLUPR
      COMMON /GCPHPR/ GFLUPR(4,MAXELZ)
C
#endif
*
* gcphpr.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcphpr
#include "geant321/incend.inc"
#endif

#endif
