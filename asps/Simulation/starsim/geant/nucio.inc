* $Id: nucio.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: nucio.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/02/27 10:15:48  nevski
*  first working release
*
      COMMON/NUCIN /TECM,AMASS(18),NPG,KGENEV
      COMMON/NUCOUT/PCM(5,18),WGT
#if !defined(CERNLIB_SINGLE)
#include "geant321/nuciod.inc"
#endif
C
