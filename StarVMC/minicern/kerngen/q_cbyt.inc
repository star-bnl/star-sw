*
* $Id: q_cbyt.inc,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: q_cbyt.inc,v $
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
* Revision 1.1.1.1  1996/02/15 17:49:19  mclareni
* Kernlib
*
*
*
* q_cbyt.inc
*
#if defined(CERNLIB_QMLNX)
      MCBYT (MZ,IZQ,IZW,IZP,NZB) = OR (
     +       AND (IZW, NOT(LSHIFT (ISHFTR(NOT(0),32-NZB),IZP-1)))
     +      ,ISHFTR (LSHIFT(LSHIFT(MZ,IZQ-1),32-NZB), 33-IZP-NZB))
#elif defined(CERNLIB_QMDOS)&&defined(CERNLIB_QF_F2C)
C      MCBYT (MZ,IZQ,IZW,IZP,NZB) = OR (
C     +          AND (IZW, NOT(LSHIFT (ishftr(NOT(0),32-NZB), IZP-1)))
C     +        , ishftr (LSHIFT(LSHIFT(MZ,-IZQ+1),32-NZB), 33-IZP-NZB) )
#elif defined(CERNLIB_QMOS9)||defined(CERNLIB_QMHPX)||(defined(CERNLIB_QMDOS)&&(!defined(CERNLIB_QF_F2C)))
      MCBYT (MZ,IZQ,IZW,IZP,NZB) = IOR (
     +          IAND (IZW, NOT(ISHFT (ISHFT(NOT(0),-32+NZB), IZP-1)))
     +        , ISHFT (ISHFT(ISHFT(MZ,-IZQ+1),32-NZB), -33+IZP+NZB) )
#endif
