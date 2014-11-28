*
* $Id: czi.inc,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: czi.inc,v $
* Revision 1.1  2009/02/25 00:08:25  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:12  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:01  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* czi.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          IZ  =  (LOCF(Z2)  - LOCF(Z)) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          IZ  =  (LOC(Z2)  - LOC(Z)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          IZ  =  (%LOC(Z2)  - %LOC(Z)) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          IZ  =  (IADDR(Z2)  - IADDR(Z)) / 8
#endif
