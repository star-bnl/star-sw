*
* $Id: mzchnb.F,v 1.5 2000/01/06 02:42:35 nevski Exp $
*
* $Log: mzchnb.F,v $
* Revision 1.5  2000/01/06 02:42:35  nevski
* elastic zebra updated
*
* Revision 1.4  1998/06/05 20:55:32  perev
* AGI commit
*
* Revision 1.3  1998/04/16 16:58:05  fisyak
* 2nd pass with gstar
*
*
#include "zebra/pilot.h"
*CMZ :          05/12/97  19.43.57  by  Pavel Nevski
*-- Author :    Pavel Nevski   05/12/97
 
      SUBROUTINE MZCHNB (LIX)
C-    Check that LIX is a link in the working space, in a link area,
C-    or even on a local variable, but not a link in a bank.
C-    This is used by routines which restore the input link
C-    from the user back to him, and which could therefor
C-    destroy bank material if a garbage collection happened
C PN: Can not be used in elastic zebra
      END

