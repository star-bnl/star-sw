* $Id: rout5.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE ROUT5(T,B,R)
      REAL*8 T(126),B(126),R(126)
C
#include "gcalor/cbert.inc"
C
      IF(NOT-2) 10 ,20 ,30
   10 CALL CRJAB(1,T(1))
      GO TO 40
C     (PIP-P)ELASTIC SCATTERING CRS.
   20 CALL CRJAB(1,B(1))
      GO TO 40
C     (PIM-P)DIRECT SCATTERING CRS.
   30 CALLCRJAB(1,R(1))
   40 RETURN
      END
