*
* $Id: cmass.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: cmass.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:45  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:44  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_CMASS_INC
#define CERNLIB_GEANT321_CMASS_INC
*
*
* cmass.inc
*
#include "geant321/camass.inc"
      DIMENSION PMASS(12)
      EQUIVALENCE(PMASS(1),XMASS(0))

#endif
