*
* $Id: gchvir.inc,v 1.1.1.1 2004/01/15 00:12:23 potekhin Exp $
*
* $Log: gchvir.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:23  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:47  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:35  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCHVIR_INC
#define CERNLIB_GEANT321_GCHVIR_INC
*
*
* gchvir.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/GCHVIR/JVIRT,JVDIV,JCONT2,JCONT
C
#if defined(CERNLIB_TYPE)
#include "geant321/gthvir.inc"
#endif
C
#endif

#endif
