*
* $Id: cdi.inc,v 1.1 2009/02/25 00:08:24 fisyak Exp $
*
* $Log: cdi.inc,v $
* Revision 1.1  2009/02/25 00:08:24  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:09  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:27:59  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:54  mclareni
* Kernlib
*
*
*
* cdi.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          ID  =  (LOCF(D2)  - LOCF(D)) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          ID  =  (LOC(D2)  - LOC(D)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          ID  =  (%LOC(D2)  - %LOC(D)) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          ID  =  (IADDR(D2)  - IADDR(D)) / 8
#endif
