*
* $Id: ztell.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: ztell.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.2  2002/10/09 19:44:48  nevski
* zebra errors in batch will stop the job
*
* Revision 1.5  2000/01/06 02:42:37  nevski
* elastic zebra updated
*
#include "zebra/pilot.h"
*CMZ :          17/10/97  12.42.42  by  Pavel Nevski
*CMZ :  1.30/00 19/03/97  22.28.33  by  Pavel Nevski
*-- Author :
      SUBROUTINE ZTELL (IDP,JFLP)
C
C-    Signal trouble situation,
C-    called from the garbage collector if not enough space
C-    may be user called with IDP > 100
C
#include "zebra/quest.inc"
      Integer         Ibatch
      Common /ACXUSER/Ibatch

      write (*,101) IDP,JFLP
 101  format(1x,20('*'),' ZEBRA problem, ZTELL called with ',2i6,
     >       1x,20('*'))
      If (IDP.eq.19 .and. JFLP.eq.0) write(*,102)
 102  format(22x,'probably there is no more space on output file')

      if (Ibatch.gt.0) STOP ' batch job impossible '
      END


