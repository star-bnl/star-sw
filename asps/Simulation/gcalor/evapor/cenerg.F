* $Id: cenerg.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION CENERG(A,Z)
C
#include "gcalor/cevcm.inc"
C
      DELTAS (A)=(A-1.0)/(1.0+(124.0/A**0.6666667))
      CAM (A,Z)=8.367*A-0.783*Z
     + -17.0354*A*(1.0-1.84619*(A-2.0*Z)**2/A**2)
     + +25.8357*A**0.6666667*(1.0-1.71219*(A-2.0*Z)**2/A**2)
     + *(1.0-0.62025/A**0.6666667)**2
     + +0.779*Z*(Z-1.0)*(1.0-1.5849/A**0.6666667+1.2273/A
     + +1.5772/A**1.3333333)/A**0.3333333
     + -0.4323*Z**1.3333333*(1.0-0.57811/A**0.3333333
     + -0.14518/A**0.6666667+0.49597/A)/A**0.3333333
      I=A
      KZ=Z
      N=A-Z
      IF(I.EQ.0) THEN
         CENERG=0.0
         RETURN
      ENDIF
      IF(N.LT.0)CALL CERROR('CENERG A<Z $')
      JPRIME=DELTAS (A)
      J=I-2*KZ-JPRIME+10
      IF(J-20)10,10,40
   10 IF(J)40,40,20
   20 IF(WAPS(I,J))30,40,30
   30 CENERG=WAPS(I,J)
      RETURN
   40 CENERG=CAM (A,Z)+CAM2(KZ)+CAM3(N)
      RETURN
      END
