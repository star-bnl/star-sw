* $Id: stopar.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE STOPAR(ID,NP)
C store particle in MPSTOR common
#include "geant321/mconst.inc"
#include "geant321/mpstor.inc"
#include "geant321/minput.inc"
C
      NPSTOR = NPSTOR + 1
      IF(NPSTOR.GT.MAXPAR) THEN
         WRITE(IOUT,'('' MICAP :  Cant store particle; bank full'',    '
     +   //'                 '' ID='',I3,'' NPSTOR='',I5)') ID,NPSTOR
         NPSTOR = NPSTOR - 1
      ELSE
         EN(NPSTOR) = EP
         UN(NPSTOR) = UP
         VN(NPSTOR) = VP
         WN(NPSTOR) = WP
         AMN(NPSTOR) = AMP
         ZMN(NPSTOR) = ZMP
         AGEN(NPSTOR) = AGEP
         MTN(NPSTOR) = MTP
         IDN(NPSTOR) = ID
         NP = NP + 1
      ENDIF
      RETURN
      END
