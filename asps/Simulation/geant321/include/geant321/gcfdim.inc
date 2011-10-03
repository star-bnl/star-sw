*
* $Id: gcfdim.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gcfdim.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:47  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:36  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCFDIM_INC
#define CERNLIB_GEANT321_GCFDIM_INC
*
*
* gcfdim.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCFDIM/NPROC,NMPTOT,TSEQTO,TOTMBY,TSEQ,
     +TLAT,TNET,X1MIN,X1MAX,Y1MIN,Y1MAX,Z1MIN,Z1MAX,
     +XCUT,YCUT,ZCUT
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtfdim.inc"
#endif
C
#endif

#endif
