*
* $Id: gcshno.inc,v 1.1.1.1 2004/07/17 20:02:49 perev Exp $
*
* $Log: gcshno.inc,v $
* Revision 1.1.1.1  2004/07/17 20:02:49  perev
* STAR version of Geant321 TGeant3 etc
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
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCSHNO_INC
#define CERNLIB_GEANT321_GCSHNO_INC
*
*
* gcshno.inc
*
#if !defined(CERNLIB_INCLUDE)
      PARAMETER ( NSBOX=1,  NSTRD1=2, NSTRD2=3, NSTRAP=4, NSTUBE=5,
     +  NSTUBS=6, NSCONE=7, NSCONS=8, NSSPHE=9, NSPARA=10,NSPGON=11,
     +  NSPCON=12,NSELTU=13,NSHYPE=14,NSGTRA=28, NSCTUB=29 )
#endif
*
* gcshno.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcshno
#include "geant321/incend.inc"
#endif

#endif
