*
* $Id: csrtli.F,v 1.1.2.1 2020/07/09 20:16:39 perev Exp $
*
* $Log: csrtli.F,v $
* Revision 1.1.2.1  2020/07/09 20:16:39  perev
* Set CERNLIB Comis files
*
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 01/10/93  16.07.47  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSRTLI(I)
***----------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
      CALL CCOPYA(ISHGI,IQ(I+2),KSIDL-2)
      END
