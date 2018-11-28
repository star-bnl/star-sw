*
* $Id: cspmd0.F,v 1.1 2018/11/19 20:44:06 perev Exp $
*
* $Log: cspmd0.F,v $
* Revision 1.1  2018/11/19 20:44:06  perev
* 64bits add files
*
* Revision 1.1.1.1  1996/02/26 17:16:26  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/02 09/07/93  15.26.07  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPMD0(M,MD,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)
      INTEGER M(10),MD(11)
      integer csvptokn
      M(1)=csvptokn(P1)
      M(2)=csvptokn(P2)
      M(3)=csvptokn(P3)
      M(4)=csvptokn(P4)
      M(5)=csvptokn(P5)
      M(6)=csvptokn(P6)
      M(7)=csvptokn(P7)
      M(8)=csvptokn(P8)
      M(9)=csvptokn(P9)
      M(10)=csvptokn(P10)
      END
