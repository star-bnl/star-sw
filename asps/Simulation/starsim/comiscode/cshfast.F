*
* $Id: cshfast.F,v 1.1.2.1 2020/07/09 20:16:33 perev Exp $
*
* $Log: cshfast.F,v $
* Revision 1.1.2.1  2020/07/09 20:16:33  perev
* Set CERNLIB Comis files
*
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :  1.18/00 08/02/94  17.48.37  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   08/02/94
      SUBROUTINE CSHFAST(LINE,EXT)
#include "comis/cshfill.inc"
      JMPHFI(LINE)=JUMPAD(EXT)
      END
#endif
