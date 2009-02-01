*
* $Id: gtscan.inc,v 1.1.1.2 2009/02/01 17:10:22 fisyak Exp $
*
* $Log: gtscan.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:22  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:22  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:34  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:34  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GTSCAN_INC
#define CERNLIB_GEANT321_GTSCAN_INC
*
*
* gtscan.inc
*
#if !defined(CERNLIB_INCLUDE)
      INTEGER MSLIST,NPHI,IPHIMI,IPHIMA,IPHI1,IPHIL,NTETA,MODTET,NSLMAX,
     +        MAXMDT,NSLIST,ISLIST,IPHI,ITETA,ISCUR
      REAL    PHIMIN,PHIMAX,TETMIN,TETMAX,VSCAN,FACTX0,FACTL,
     +        FACTR,SX0,SABS,TETMID,TETMAD
     +       ,SX0S,SX0T,SABSS,SABST,FACTSF
     +       ,DLTPHI,DLTETA,DPHIM1,DTETM1
     +       ,FCX0M1,FCLLM1,FCRRM1
#endif

#endif
