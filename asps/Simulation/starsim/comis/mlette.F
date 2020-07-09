*
* $Id: mlette.F,v 1.1.2.1 2020/07/09 19:44:03 perev Exp $
*
* $Log: mlette.F,v $
* Revision 1.1.2.1  2020/07/09 19:44:03  perev
* Add64
*
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          24/04/95  18.15.17  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MLETTE(L)
#include "comis/csichv.inc"
      MLETTE=0
      IF(L.GE.ICHLA .AND. L.LE.ICHLZ .OR. L.EQ.ICHSUSC)MLETTE=1
      END
