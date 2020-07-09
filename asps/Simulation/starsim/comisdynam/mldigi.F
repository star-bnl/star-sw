*
* $Id: mldigi.F,v 1.1.2.1 2020/07/09 20:22:03 perev Exp $
*
* $Log: mldigi.F,v $
* Revision 1.1.2.1  2020/07/09 20:22:03  perev
* Set CERNLIB Comis files
*
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.18/00 16/02/94  15.34.30  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MLDIGI(I)
#include "comis/csichv.inc"
      IF(I.GE.ICHD0 .AND. I.LE.ICHD9)THEN
        MLDIGI=1
      ELSE
        MLDIGI=0
      ENDIF
      END
#endif
