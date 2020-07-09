*
* $Id: csputl.F,v 1.1.2.1 2020/07/09 20:16:39 perev Exp $
*
* $Log: csputl.F,v $
* Revision 1.1.2.1  2020/07/09 20:16:39  perev
* Set CERNLIB Comis files
*
* Revision 1.1.1.1  1996/02/26 17:16:30  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.17/07 15/12/93  18.24.03  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   15/12/93
      SUBROUTINE CSPUTL(IND,LINE,NCH,MARK)
      CHARACTER *(*)LINE
      COMMON/MDPOOL/IQ(1000)
      NW=(NCH+3)/4+3
      I=MHLOC(NW)
      IQ(I)=0
      IF(IND.GT.0)IQ(IND)=I
      IND=I
      IQ(I+1)=MARK
      IQ(I+2)=NCH
      CALL UCTOH(LINE,IQ(I+3),4,NCH)
      END
