* $Id: clear.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CLEAR(L,L1,L2)
C       THIS ROUTINE ZEROS ARRAY L FROM
C       STARTING POINT L1 TO ENDING POINT L2
      DIMENSION L(*)
      IF(L2-L1.LT.0)GO TO 20
      DO 10 I=L1,L2
   10 L(I)=0
   20 RETURN
      END
