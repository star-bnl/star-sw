*
* $Id: q_isabit.inc,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: q_isabit.inc,v $
* Revision 1.1.1.1  2004/07/17 20:01:57  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:46  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:29  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:49:20  mclareni
* Kernlib
*
*
*
* q_isabit.inc
*
#if defined(CERNLIB_QMLNX)||(defined(CERNLIB_QMDOS)&&defined(CERNLIB_QF_F2C))
      BTEST(IZW,IZP) = AND (IZW,LSHIFT(1,IZP)) .NE. 0
      IBSET(IZW,IZP) =  OR (IZW,LSHIFT(1,IZP))
      IBCLR(IZW,IZP) = AND (IZW,NOT(LSHIFT(1,IZP)))
      IBITS (IZW,IZP,NZB) = ISHFTR (LSHIFT(IZW,32-IZP-NZB), 32-NZB)
#endif
