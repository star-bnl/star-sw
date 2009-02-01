*
* $Id: mpstor.inc,v 1.1.1.2 2009/02/01 17:10:22 fisyak Exp $
*
* $Log: mpstor.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:22  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:28  fisyak
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
* Revision 1.1.1.1  1999/05/18 15:55:17  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:45  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_MPSTOR_INC
#define CERNLIB_GEANT321_MPSTOR_INC
*
*
* mpstor.inc
*
      PARAMETER(IDNEU  = 1)
      PARAMETER(IDHEVY = 2)
      PARAMETER(IDGAMA = 3)
      COMMON/ MPSTOR / EN(MAXPAR),UN(MAXPAR),VN(MAXPAR),WN(MAXPAR),
     +                 AGEN(MAXPAR),MTN(MAXPAR),AMN(MAXPAR),
     +                 ZMN(MAXPAR),IDN(MAXPAR),
     +                 EP,UP,VP,WP,MTP,AGEP,AMP,ZMP,
     +                 NNEU,NHEVY,NGAMA,NPSTOR

#endif
