*
* $Id: ibits.F,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: ibits.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:57  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:47  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:30  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:49:58  mclareni
* Kernlib
*
*
      FUNCTION IBITS (IZW,IZP,NZB)
*
      IBITS=ISHFTR(LSHIFT(IZW,32-IZP-NZB),32-NZB)
*
      RETURN
      END
