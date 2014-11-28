*
* $Id: cyij.inc,v 1.1 2009/02/25 00:08:25 fisyak Exp $
*
* $Log: cyij.inc,v $
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
* cyij.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          IY  =  (LOCF(Y21) - LOCF(Y)) / 2
          JY  =  (LOCF(Y12) - LOCF(Y)) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          IY  =  (LOC(Y21) - LOC(Y)) / 2
          JY  =  (LOC(Y12) - LOC(Y)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          IY  =  (%LOC(Y21) - %LOC(Y)) / 8
          JY  =  (%LOC(Y12) - %LOC(Y)) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          IY  =  (IADDR(Y21) - IADDR(Y)) / 8
          JY  =  (IADDR(Y12) - IADDR(Y)) / 8
#endif
