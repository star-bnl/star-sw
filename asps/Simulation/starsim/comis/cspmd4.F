*
* $Id: cspmd4.F,v 1.2 2018/12/03 00:49:08 perev Exp $
*
* $Log: cspmd4.F,v $
* Revision 1.2  2018/12/03 00:49:08  perev
* Replace csvptokn to locb
*
* Revision 1.1  2018/11/19 20:44:06  perev
* 64bits add files
*
* Revision 1.1.1.1  1996/02/26 17:16:26  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 09/07/93  15.31.47  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPMD4(M,MD,P1,P2,P3,P4)
      INTEGER M(10),MD(11)
      integer locb
      M(1)=locb(P1)
      M(2)=locb(P2)
      M(3)=locb(P3)
      M(4)=locb(P4)
      END
