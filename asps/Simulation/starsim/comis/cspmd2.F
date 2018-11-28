*
* $Id: cspmd2.F,v 1.1 2018/11/19 20:44:06 perev Exp $
*
* $Log: cspmd2.F,v $
* Revision 1.1  2018/11/19 20:44:06  perev
* 64bits add files
*
* Revision 1.1.1.1  1996/02/26 17:16:26  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 09/07/93  15.30.03  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPMD2(M,MD,P1,P2)
      INTEGER M(10),MD(11)
      integer csvptokn
      M(1)=csvptokn(P1)
      M(2)=csvptokn(P2)
      END
