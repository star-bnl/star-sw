*
* $Id: mzchnb.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: mzchnb.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.2  2001/06/15 23:00:20  nevski
* if-endif corrected
*
* Revision 1.1  2001/02/27 10:15:25  nevski
* elastic zebra updated
*
#include "zebra/pilot.h"
*
      SUBROUTINE MZCHNB (LIX)
C-    Check that LIX is a link in the working space, in a link area,
C-    or even on a local variable, but not a link in a bank.
C-    This is used by routines which restore the input link
C-    from the user back to him, and which could therefor
C-    destroy bank material if a garbage collection happened
C- PN: Can not be used in elastic zebra
      END
