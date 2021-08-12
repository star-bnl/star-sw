*
* $Id: crandm.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: crandm.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:45  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:44  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_CRANDM_INC
#define CERNLIB_GEANT321_CRANDM_INC
*
*
* crandm.inc
*
#if !defined(CERNLIB_SINGLE)
      DOUBLE PRECISION RANDC
C
#endif
#if defined(CERNLIB_SINGLE)
      REAL RANDC
C
#endif

#endif
