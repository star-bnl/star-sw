*
* $Id: dzij.inc,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: dzij.inc,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:15  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:07  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* dzij.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          IZ  =  (LOCF(Z21) - LOCF(Z)) / 2
          JZ  =  (LOCF(Z12) - LOCF(Z)) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          IZ  =  (LOC(Z21) - LOC(Z)) / 2
          JZ  =  (LOC(Z12) - LOC(Z)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          IZ  =  (%LOC(Z21) - %LOC(Z)) / 8
          JZ  =  (%LOC(Z12) - %LOC(Z)) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          IZ  =  (IADDR(Z21) - IADDR(Z)) / 8
          JZ  =  (IADDR(Z12) - IADDR(Z)) / 8
#endif
