*
* $Id: gcphnr.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcphnr.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:48  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:35  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCPHNR_INC
#define CERNLIB_GEANT321_GCPHNR_INC
*
*
* gcphnr.inc
*
#if !defined(CERNLIB_INCLUDE)
C  INRFIN - nonradiative decay mode
      INTEGER IGNRFN
      COMMON /GCPHNR/ IGNRFN(8,MAXELZ)
C
#endif
*
* gcphnr.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gcphnr
#include "geant321/incend.inc"
#endif

#endif
