* $Id: intsch.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE INTSCH(IFSE,I,IS,NR)
C       THIS ROUTINE DETERMINES THE INTERPOLATION SCHEME
C       ACCORDING TO ENDF/B-V FORMATTED DATA FILES
      DIMENSION IFSE(*)
      DO 10 J=1,NR
         J1=3+2*(J-1)
         NPTS=IFSE(J1)
         IF(I.LE.NPTS)GO TO 20
   10 CONTINUE
   20 IS=IFSE(J1+1)
      RETURN
      END
