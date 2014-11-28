*
* $Id: ryj.inc,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: ryj.inc,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:19  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:18  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* ryj.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          JY  =   LOCF(Y2) - LOCF(Y)
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          JY  =   LOC(Y2) - LOC(Y)
#endif
#if defined(CERNLIB_NUMDE)
          JY  =   (%LOC(Y2) - %LOC(Y)) / 4
#endif
#if defined(CERNLIB_NUMIB1)
          JY  =   (IADDR(Y2) - IADDR(Y)) / 4
#endif
