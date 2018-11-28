*
* $Id: miword.F,v 1.1 2018/11/19 20:44:55 perev Exp $
*
* $Log: miword.F,v $
* Revision 1.1  2018/11/19 20:44:55  perev
* 64bits add files
*
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MIWORD(JW)
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
      integer mask/1073741824/ !0x40000000
      call assert (jw<0 .or. iand(jw,mask).eq.0);
      MIWORD=IA(JW-JTOPA)
      END
