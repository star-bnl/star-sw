*
* $Id: gcphpr.inc,v 1.1.1.2 2009/02/01 17:10:21 fisyak Exp $
*
* $Log: gcphpr.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:21  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:15  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:32  fisyak
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
