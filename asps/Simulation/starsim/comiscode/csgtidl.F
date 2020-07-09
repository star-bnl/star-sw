*
* $Id: csgtidl.F,v 1.1.2.1 2020/07/09 20:16:33 perev Exp $
*
* $Log: csgtidl.F,v $
* Revision 1.1.2.1  2020/07/09 20:16:33  perev
* Set CERNLIB Comis files
*
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 01/10/93  09.53.14  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSGTIDL(IND,NAME,NC)
***--------------------------------------
      CHARACTER *(*)NAME
#include "comis/cstabps.inc"
      CALL CSGTID(IND,KSIDL,NAME,NC)
      END
