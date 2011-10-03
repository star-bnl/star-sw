*
* $Id: gcstra.inc,v 1.1.1.1 2004/01/15 00:12:24 potekhin Exp $
*
* $Log: gcstra.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:24  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCSTRA_INC
#define CERNLIB_GEANT321_GCSTRA_INC
*
*
* gcstra.inc
*
#if !defined(CERNLIB_INCLUDE)
      PARAMETER (ILTAB=200)
      COMMON /GCSTRA / EMAX,EM(200),SFINT,EPSR(ILTAB),EPSI(ILTAB),
     +  FINT(ILTAB),EMIN,EPPS,BETA2,GAMMA2,WP2,S2,MEEV,EMM(200),
     +  GAMLOG(21),NP,NTAB,IE,NFACT,NICOLL
*
      INTEGER NP,NTAB,IE,NFACT
      REAL    EMAX,EM,SFINT,EPSR,EPSI,FINT,EMIN,EPPS,EMM,GAMLOG
     +        ,BETA2,GAMMA2,WP2,S2,MEEV
*
#endif

#endif
