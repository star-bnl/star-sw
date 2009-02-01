*
* $Id: gcflag.inc,v 1.1.1.2 2009/02/01 17:10:20 fisyak Exp $
*
* $Log: gcflag.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:20  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:12  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:29  fisyak
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
* Revision 1.1.1.1  1995/10/24 10:20:31  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCFLAG_INC
#define CERNLIB_GEANT321_GCFLAG_INC
*
*
* gcflag.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtflag.inc"
#endif
#endif
*
* gcflag.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcflag
#include "geant321/incend.inc"
#endif

#endif
