* $Id: czfoi.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION CZFOI(Z)
C**   USING REVISED DATA (1-70,TWA)
      DIMENSION FLI(13)
      DATA FLI/18.7,42.0,39.0,60.0,68.0,78.0,99.5,98.5,117.0,140.0,
     1150.0,157.0,163.0/,A1/9.76/,A2/58.8/,XP/-0.19/
      IF(Z.GT.13.) GO TO 10
      IZ=IFIX(Z)
      CZFOI=FLI(IZ)
      RETURN
   10 CZFOI=A1*Z + A2 *(Z**XP)
      RETURN
      END
