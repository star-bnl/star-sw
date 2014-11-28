*
* $Id: rujk.inc,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rujk.inc,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:18  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:18  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:54  mclareni
* Kernlib
*
*
*
* rujk.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          JU  =  LOCF(U12) - LOCF(U)
          KU  =  LOCF(U22) - LOCF(U)
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          JU  =  LOC(U12) - LOC(U)
          KU  =  LOC(U22) - LOC(U)
#endif
#if defined(CERNLIB_NUMDE)
          JU  =  (%LOC(U12) - %LOC(U)) / 4
          KU  =  (%LOC(U22) - %LOC(U)) / 4
#endif
#if defined(CERNLIB_NUMIB1)
          JU  =  (IADDR(U12) - IADDR(U)) / 4
          KU  =  (IADDR(U22) - IADDR(U)) / 4
#endif
