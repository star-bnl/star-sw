* $Id: gncsto.inc,v 1.1.1.1 2004/01/12 23:49:37 potekhin Exp $
* $Log: gncsto.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:37  potekhin
*
* Revision 1.1  2001/02/27 10:14:16  nevski
*  first working release
*
#include "geant321/gcbank.inc"
      real      GN(1)
      integer   IGN(1),LGN
*     external  IGN,LGN
      integer   IGNOFF/0/
      integer   ignsto,LENB
      character CTYP*4
      common   /gncsto/ ignsto,LENB,CTYP
      EQUIVALENCE (Q,GN), (IQ,IGN)
