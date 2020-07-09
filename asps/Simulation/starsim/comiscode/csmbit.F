*
* $Id: csmbit.F,v 1.1.2.1 2020/07/09 20:16:37 perev Exp $
*
* $Log: csmbit.F,v $
* Revision 1.1.2.1  2020/07/09 20:16:37  perev
* Set CERNLIB Comis files
*
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.14/05 03/06/93  10.19.28  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSMBIT(MFROM,MPOS,LEN,LTO,LPOS)
      CALL MVBITS(MFROM,MPOS,LEN,LTO,LPOS)
      END
