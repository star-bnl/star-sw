* $Id: xseche.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION XSECHE(ID,ITP,EC)
#include "gcalor/cbert2.inc"
#include "gcalor/cxpd.inc"
C
C   ID =   1          2         3         4
C      SNGL PROD  DBLE PROD  ELASTIC  EXCHANGE
C  ITP =   1          2         3         4
C        PROT       NEUT       PI+       PI-
      ET = ETH(ID,ITP)
      LC =LOCX(ID,ITP)
      I= IFIX( (EC-ET)/DE + 1.)
      E = FLOAT(I-1)*DE + ET
      TEMP = SNGL( CS(I+LC) )
      XSECHE=TEMP + (EC-E)/DE * ( SNGL( CS(I+1+LC) ) - TEMP )
      RETURN
      END
