*
* $Id: mlsges.F,v 1.1.2.1 2020/07/09 20:22:04 perev Exp $
*
* $Log: mlsges.F,v $
* Revision 1.1.2.1  2020/07/09 20:22:04  perev
* Set CERNLIB Comis files
*
* Revision 1.1.1.1  1996/02/26 17:16:58  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if (defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB))&&(defined(CERNLIB_LIB))
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLSGES(J1,N1,J2,N2)
      IF(N1.GE.N2) GO TO 1
      MLSGES=0
      RETURN
    1 MLSGES=MLEQS(J1,J2,N2)
      RETURN
      END
#endif
