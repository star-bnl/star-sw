* $Id: cadig.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION CADIG(E)
C       THIS FUNCTION ADDS A TOLERANCE TO THE ARGUMENT
      ARG=ALOG10(E)
      ITR=5-IFIX(ARG)
      EPS=10.**ITR
      CADIG=1./EPS
      RETURN
      END
