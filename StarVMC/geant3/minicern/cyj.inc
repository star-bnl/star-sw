*
* $Id: cyj.inc,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: cyj.inc,v $
* Revision 1.1  2009/02/25 00:08:25  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:12  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:01  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* cyj.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          JY  =  (LOCF(Y2) - LOCF(Y)) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          JY  =  (LOC(Y2) - LOC(Y)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JY  =  (%LOC(Y2) - %LOC(Y)) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          JY  =  (IADDR(Y2) - IADDR(Y)) / 8
#endif
