*
* $Id: gcvdma.inc,v 1.1.1.1 2004/01/15 00:12:24 potekhin Exp $
*
* $Log: gcvdma.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:24  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:36  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCVDMA_INC
#define CERNLIB_GEANT321_GCVDMA_INC
*
*
* gcvdma.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCVDMA/NVMANY,MANYLE(20),MANYNA(20,15),
     +MANYNU(20,15),NFMANY,MYCOUN,IMYSE,RAYTRA,VECCOS(3)
C
#if defined(CERNLIB_TYPE)
#include "geant321/gtvdma.inc"
#endif
C
#endif

#endif
