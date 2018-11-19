*
* $Id: mhloc.F,v 1.2 2018/11/19 20:44:06 perev Exp $
*
* $Log: mhloc.F,v $
* Revision 1.2  2018/11/19 20:44:06  perev
* 64bits add files
*
* Revision 1.1  2004/09/24 18:57:04  fisyak
* Fix comis for Scientific Linux
*
* Revision 1.6  1997/10/23 11:49:49  mclareni
* Simplification on all platforms, necessary for NT
*
* Revision 1.5  1997/07/18 15:26:36  berejnoi
* using displacement from mdpool
*
* Revision 1.4  1997/07/14 11:30:23  couet
* - Use again csfree for sgi and linux.
*
* Revision 1.3  1997/07/08 12:59:54  couet
* - go back to the old dynamic memory allocation mechanism for LINUX. The new
*   one does not work: it hangs in mhfree.
*
* Revision 1.2  1997/06/03 08:50:25  berejnoi
* using malloc
*
* Revision 1.1.1.1  1996/02/26 17:16:59  mclareni
* Comis
*
*
#include "comis/pilot.h"
*-- Author : V.Berezhnoi
*-- Author : V.Perev
      FUNCTION MHLOC(N)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/mdloc.inc"
      integer fist/0/
      save fist
*----------------------------------
      if (fist.eq.0) then
        IQ(1) = 0
        fist = 1946
	IQlast = 0
	IQfree = 0
      endif
      call assert(n.le.kPAGE)

      if (IQfree .NE. 0 ) then
        MHLOC=IQfree
        IQfree = IQ(IQfree)
      else
        call assert(IQlast+kPAGE .lt. LHP)
        MHLOC=IQlast+1
        IQlast = IQlast + kPAGE
      endif
      return
      END
