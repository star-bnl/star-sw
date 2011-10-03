*
* $Id: getenvf.F,v 1.1.1.1 2004/07/17 20:01:56 perev Exp $
*
* $Log: getenvf.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:56  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:46  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:28  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:49:22  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QMVAX)
#include "vaxsys/getenvf.F"
#else
      SUBROUTINE GETENVF (NAME, TEXT)
C
C CERN PROGLIB# Z265    GETENVF         .VERSION KERNFOR  4.26  910313
C ORIG. 22/02/91, JZ
C Fortran interface routine to getenv
C
      COMMON /SLATE/ISL(40)
      CHARACTER*(*)  NAME, TEXT

      LGN = LNBLNK (NAME)
      NTX = LEN(TEXT)
      CALL GETENI (NAME,TEXT,NTX,LGN)
      ISL(1) = NTX
      RETURN
      END
#endif
