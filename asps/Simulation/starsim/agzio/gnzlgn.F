* $Id: gnzlgn.F,v 1.1.1.1 2004/01/12 23:49:37 potekhin Exp $
* $Log: gnzlgn.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:37  potekhin
*
* Revision 1.2  2001/03/02 21:03:13  nevski
* agmer splitted, gnz restored
*
* Revision 1.1  2001/02/27 10:14:17  nevski
*  first working release
*
       function LGN(i)
#include "gncsto.inc"
#include "slug/sclink.inc"

C           IF(ILINK.EQ.1)THEN
C         LPQR = LGN(IGNLKP(1))
C         LPQE = LGN(IGNLKP(2))
C      ELSE IF(ILINK.EQ.2)THEN
C         LPQR = LGN(IGNLKP(3))
C         LPQE = LGN(IGNLKP(4))

       L    = 0
       LENB = 0
       if (0.lt.i .and. i.lt.5) then
          ide    = (i+1)/2
          m      = mod(i,2)
          ignsto = IxDiv
          if (m.eq.1)  L=LZFIND(IxDiv,LkRunt,IDE,-5)
          if (m.eq.0)  L=LZFIND(IXDIV,LkEvnt,IDE,-5)
          if (L.gt.0)  CALL UHTOC(IQ(L-4),4,CTYP,4)
          if (L.gt.0)  LENB =     IQ(L-1)
       else if (i.gt.1000) then
          L=LQ(i)
       endif
          LGN=L
       end

       function IGNLKP(i)
       IGNLKP=i
       end


*================================================================
