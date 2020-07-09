*
* $Id: csrtgp.F,v 1.1.2.1 2020/07/09 20:16:39 perev Exp $
*
* $Log: csrtgp.F,v $
* Revision 1.1.2.1  2020/07/09 20:16:39  perev
* Set CERNLIB Comis files
*
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 10/01/95  15.41.06  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSRTGP(I)
***--------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
      CALL CCOPYA(IADGP,IQ(I+2),KSIDP-2)
      END
