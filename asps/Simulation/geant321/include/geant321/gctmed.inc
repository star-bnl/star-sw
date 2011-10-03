*
* $Id: gctmed.inc,v 1.1.1.1 2004/01/15 00:12:24 potekhin Exp $
*
* $Log: gctmed.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:24  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCTMED_INC
#define CERNLIB_GEANT321_GCTMED_INC
*
*
* gctmed.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
#if defined(CERNLIB_TYPE)
#include "geant321/gttmed.inc"
#endif
#endif
*
* gctmed.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gctmed
#include "geant321/incend.inc"
#endif

#endif
