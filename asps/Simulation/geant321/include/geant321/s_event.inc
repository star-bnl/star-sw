*
* $Id: s_event.inc,v 1.1.1.1 2004/01/15 00:12:30 potekhin Exp $
*
* $Log: s_event.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:30  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:55  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:44  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_S_EVENT_INC
#define CERNLIB_GEANT321_S_EVENT_INC
*
*
* s_event.inc
*
      PARAMETER (MXEVEN=12*MXGKGH)
      COMMON/EVENT / NSIZE,NCUR,NEXT,NTOT,EVE(MXEVEN)
C

#endif
