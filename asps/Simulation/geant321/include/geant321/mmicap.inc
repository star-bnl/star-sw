*
* $Id: mmicap.inc,v 1.1.1.1 2004/01/15 00:12:30 potekhin Exp $
*
* $Log: mmicap.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:30  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:52  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:45  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_MMICAP_INC
#define CERNLIB_GEANT321_MMICAP_INC
*
*
* mmicap.inc
*
C
#include "geant321/mmicab.inc"
#include "geant321/gcbank.inc"
      DIMENSION LD(1),D(1)
      EQUIVALENCE (D(1),Q(1)),(LD(1),IQ(1))
      CHARACTER*24 DATSTR
      CHARACTER*80 COMMEN
      COMMON/ MICMAT / DATSTR,COMMEN,MATIDS(100,20,2)
C

#endif
