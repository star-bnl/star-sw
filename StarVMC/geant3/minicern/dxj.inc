*
* $Id: dxj.inc,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dxj.inc,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:14  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:04  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* dxj.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          JX  =  (LOCF(X2) - LOCF(X)) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          JX  =  (LOC(X2) - LOC(X)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          JX  =  (IADDR(X2) - IADDR(X)) / 8
#endif
