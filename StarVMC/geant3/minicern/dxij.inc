*
* $Id: dxij.inc,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dxij.inc,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:14  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:04  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* dxij.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          IX  =  (LOCF(X21) - LOCF(X)) / 2
          JX  =  (LOCF(X12) - LOCF(X)) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          IX  =  (LOC(X21) - LOC(X)) / 2
          JX  =  (LOC(X12) - LOC(X)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          IX  =  (%LOC(X21) - %LOC(X)) / 8
          JX  =  (%LOC(X12) - %LOC(X)) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          IX  =  (IADDR(X21) - IADDR(X)) / 8
          JX  =  (IADDR(X12) - IADDR(X)) / 8
#endif
