*
* $Id: rdi.inc,v 1.1 2009/02/25 00:08:27 fisyak Exp $
*
* $Log: rdi.inc,v $
* Revision 1.1  2009/02/25 00:08:27  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:17  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:16  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:54  mclareni
* Kernlib
*
*
*
* rdi.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          ID  =  LOCF(D2)  - LOCF(D)
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          ID  =  LOC(D2)  - LOC(D)
#endif
#if defined(CERNLIB_NUMDE)
          ID  =  (%LOC(D2)  - %LOC(D)) / 4
#endif
#if defined(CERNLIB_NUMIB1)
          ID  =  (IADDR(D2)  - IADDR(D)) / 4
#endif
