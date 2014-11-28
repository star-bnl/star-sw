*
* $Id: cxi.inc,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: cxi.inc,v $
* Revision 1.1  2009/02/25 00:08:25  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:12  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:01  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:54  mclareni
* Kernlib
*
*
*
* cxi.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          IX  =  (LOCF(X2)  - LOCF(X)) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          IX  =  (LOC(X2)  - LOC(X)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          IX  =  (%LOC(X2)  - %LOC(X)) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          IX  =  (IADDR(X2)  - IADDR(X)) / 8
#endif
