* $Id: p1clc.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE P1CLC
#include "gcalor/cbert.inc"
C
      P1OE1=DSQRT(E(1)*E(1)-PM(1)*PM(1))
      PXYZ(1)=P1OE1*CURR(7)
      PXYZ(5)=P1OE1*CURR(8)
      PXYZ(9)=P1OE1*CURR(9)
      P1OE1=P1OE1/E(1)
      RETURN
      END
