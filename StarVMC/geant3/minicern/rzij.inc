*
* $Id: rzij.inc,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rzij.inc,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:19  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:18  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* rzij.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          IZ  =  LOCF(Z21) - LOCF(Z)
          JZ  =  LOCF(Z12) - LOCF(Z)
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          IZ  =  LOC(Z21) - LOC(Z)
          JZ  =  LOC(Z12) - LOC(Z)
#endif
#if defined(CERNLIB_NUMDE)
          IZ  =  (%LOC(Z21) - %LOC(Z)) / 4
          JZ  =  (%LOC(Z12) - %LOC(Z)) / 4
#endif
#if defined(CERNLIB_NUMIB1)
          IZ  =  (IADDR(Z21) - IADDR(Z)) / 4
          JZ  =  (IADDR(Z12) - IADDR(Z)) / 4
#endif
