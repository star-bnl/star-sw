*
* $Id: jbyt.F,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: jbyt.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:57  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  1999/05/18 15:55:32  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:50:20  mclareni
* Kernlib
*
*
      FUNCTION JBYT (IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JBYT            .VERSION KERNSUN  1.06  920511
C ORIG. 22/09/88, JZ
C
#if defined(CERNLIB_BUGLRSHFT)
      JBYT = ishft (LSHIFT(IZW,33-IZP-NZB),-32+NZB)
#endif
#if !defined(CERNLIB_BUGLRSHFT)
      JBYT = lrshft (LSHIFT(IZW,33-IZP-NZB), 32-NZB)
#endif
      RETURN
      END
