/*
* $Id: wordsizc.h,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: wordsizc.h,v $
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
* wordsizc.h
*/
#if defined(CERNLIB_QMIRTD)
#define NBITPW 64      /* Number of bits  per word */
#define NBYTPW 8       /* Number of bytes per word */
#else
#define NBYTPW 4       /* Number of bytes per word */
#endif
