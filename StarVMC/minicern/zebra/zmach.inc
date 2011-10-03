*
* $Id: zmach.inc,v 1.1.1.1 2004/07/17 20:01:58 perev Exp $
*
* $Log: zmach.inc,v $
* Revision 1.1.1.1  2004/07/17 20:01:58  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:50  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:28  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 10:46:52  mclareni
* Zebra
*
*
#ifndef CERNLIB_ZEBRA_ZMACH_INC
#define CERNLIB_ZEBRA_ZMACH_INC
*
*                Machine parameters
*
* zmach.inc
*
#include "zebra/zmachfix.inc"
      COMMON /ZMACH/ NQBITW,NQBITC,NQCHAW
     +,              NQLNOR,NQLMAX,NQLPTH,NQRMAX,IQLPCT,IQNIL

#endif
