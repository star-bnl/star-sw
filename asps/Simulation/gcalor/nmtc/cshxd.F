* $Id: cshxd.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CSHXD
C
#include "gcalor/cxpd.inc"
C
      DE = 20.
      LOCX(1,1) = 995
      LOCX(2,1) = 1153
      LOCX(3,1) = 3793
      LOCX(4,1)= 0
      LOCX(1,2) = 1283
      LOCX(2,2) = 1441
      LOCX(3,2) = 3969
      LOCX(4,2)= 0
      LOCX(1,3) = 2009
      LOCX(2,3)= 0
      LOCX(3,3) = 3667
      LOCX(4,3)= 0
      LOCX(1,4) = 2243
      LOCX(2,4)= 0
      LOCX(3,4) = 3541
      LOCX(4,4) = 3415
      DO 10 IT =1,4
         DO 10 ID =1,4
   10 ETH(ID,IT) = 0.
      DO 20 IT=1,2
         ETH(1,IT)= 360.
   20 ETH(2,IT)= 920.
      DO 30  IT =3,4
   30 ETH(1,IT)= 180.
      RETURN
      END
