*
* $Id: rgmlt.F,v 1.1 2009/02/25 00:08:27 fisyak Exp $
*
* $Log: rgmlt.F,v $
* Revision 1.1  2009/02/25 00:08:27  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:17  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:17  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/04/01 15:02:15  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_DOUBLE)
      FUNCTION RGMLT1(FSUB1,A,B,NI,NG,X)

      ENTRY RGMLT2(FSUB2,A,B,NI,NG,X)
      ENTRY RGMLT3(FSUB3,A,B,NI,NG,X)
      ENTRY RGMLT4(FSUB4,A,B,NI,NG,X)
      ENTRY RGMLT5(FSUB5,A,B,NI,NG,X)
      ENTRY RGMLT6(FSUB6,A,B,NI,NG,X)

C
      CHARACTER NAME*(*)
      PARAMETER (NAME = 'RGMLT')
      CALL MTLPRT(NAME,'D110.0','Not available on this machine')
      RGMLT1=0
      RETURN
      END
#endif
