* $Id: cqnrg.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION CQNRG(A1,Z1,A2,Z2)
C
#include "gcalor/cevcm.inc"
C
      DELTAS (A)=(A-1.0)/(1.0+(124.0/A**0.6666667))
      CAM (A,Z)=8.367*A-0.783*Z
     1-17.0354*A*(1.0-1.84619*(A-2.0*Z)**2/A**2)
     2+25.8357*A**0.6666667*(1.0-1.71219*(A-2.0*Z)**2/A**2)
     3*(1.0-0.62025/A**0.6666667)**2
     4+0.779*Z*(Z-1.0)*(1.0-1.5849/A**0.6666667+1.2273/A
     5+1.5772/A**1.3333333)/A**0.3333333
     6-0.4323*Z**1.3333333*(1.0-0.57811/A**0.3333333
     7-0.14518/A**0.6666667+0.49597/A)/A**0.3333333
      I1 = A1
      I2 = A2
      KZ1 = Z1
      KZ2 = Z2
      N1 = A1 - Z1
      N2 = A2 - Z2
      IF(N1.LE.0) CALL CERROR('CQNRG1$')
      IF(N2.LE.0) CALL CERROR('CQNRG2$')
      JP1 = DELTAS(A1)
      JP2 = DELTAS(A2)
      J1 = I1 - 2*KZ1 - JP1 + 10
      J2 = I2 - 2*KZ2 - JP2 + 10
      IF(J1.LT.1.OR.J1.GT.20) GO TO 10
      IF(J2.LT.1.OR.J2.GT.20) GO TO 10
      ENRG1 = WAPS(I1,J1)
      ENRG2 = WAPS(I2,J2)
      IF(ENRG1.EQ.0.) GO TO 10
      IF(ENRG2.EQ.0.) GO TO 10
      GO TO 20
   10 ENRG1 = CAM(A1,Z1) + CAM2(KZ1) + CAM3(N1)
      ENRG2 = CAM(A2,Z2) + CAM2(KZ2) + CAM3(N2)
   20 CQNRG = ENRG1 - ENRG2
      RETURN
      END
