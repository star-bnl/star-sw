*
* $Id: ztell.F,v 1.1 1998/04/10 15:31:14 fisyak Exp $
*
* $Log: ztell.F,v $
* Revision 1.1  1998/04/10 15:31:14  fisyak
* Move gstar into CVS
*
*
#include "graflib/dzdoc/dzdoc/pilot.h"
*CMZ :          17/10/97  12.42.42  by  Pavel Nevski
*CMZ :  1.30/00 19/03/97  22.28.33  by  Pavel Nevski
*-- Author :
      SUBROUTINE ZTELL (IDP,JFLP)
#include "gst/atcdes/quest.inc"
C-    Signal trouble situation,
C-    called from the garbage collector if not enough space
C-    may be user called with IDP > 100
      write (*,101) IDP,JFLP
 101  format(1x,20('*'),' ZEBRA problem, ZTELL called with ',2i6,
     >       1x,20('*'))
      If (IDP.eq.19 .and. JFLP.eq.0) write(*,102)
 102  format(22x,'probably there is no more space on output file')
      END
 
 
 
 
 
