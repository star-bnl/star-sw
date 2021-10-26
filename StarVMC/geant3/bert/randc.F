* $Id: randc.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      DOUBLE PRECISION FUNCTION RANDC(DUMMY)
C
      DIMENSION RND1(1)
C
      CALL GRNDM(RND1,1)
      RANDC = RND1(1)
      RETURN
      END
