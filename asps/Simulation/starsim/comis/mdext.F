*
* $Id: mdext.F,v 1.2.4.1 2020/08/07 02:52:16 perev Exp $
*
* $Log: mdext.F,v $
* Revision 1.2.4.1  2020/08/07 02:52:16  perev
* CleanUp
*
* Revision 1.2  2020/06/04 23:05:02  perev
* add csguard call
*
* Revision 1.1  2004/09/24 18:57:04  fisyak
* Fix comis for Scientific Linux
*
* Revision 1.1.1.1  1996/02/26 17:16:48  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 08/07/93  18.32.13  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      FUNCTION MDEXT(LENG,IND)
      COMMON/MDPOOL/IQ(6)
      LOGICAL MDEXT
#if defined(CERNLIB_UNIX)

      INTEGER CSALLO
      L=LENG*4
      IM=CSALLO(L)
      IF(IM.EQ.0)THEN
         MDEXT=.FALSE.
      ELSE
         MDEXT=.TRUE.
*yf 092404         IND=(IM-LOCB(IQ(1))+4)/4
         IND=IM+(-LOCB(IQ(1))+4)/4
      ENDIF
#endif
      END
