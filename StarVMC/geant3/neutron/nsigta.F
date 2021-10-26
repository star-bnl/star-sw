* $Id: nsigta.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE NSIGTA(E,JMED,TSIG,D,ISIGTS,LSIGT)
C       THIS ROUTINE DETERMINES THE MACROSCOPIC TOTAL
C       CROSS SECTION FOR MEDIA MED
#include "geant321/mmicab.inc"
      DIMENSION D(*),ISIGTS(*),LSIGT(*)
      CALL GTMED(JMED,MED)
      TSIG=0.0
      L1=LSIGT(MED)
      LS1=ISIGTS(MED)+LMOX3
      LEN=L1/2
      CALL TBSPLT(D(LS1),E,LEN,TSIG)
      RETURN
      END
