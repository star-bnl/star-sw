*
* $Id: gavrl2.F,v 1.1.1.2 2009/02/01 17:10:30 fisyak Exp $
*
* $Log: gavrl2.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:30  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:32  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:54  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:40  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:20  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:21  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      REAL FUNCTION G3AVRL2(GAMA,BETA,XM)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  L2 shell x-section a la Gavrila                               *
C.    *    Gama  - L2 shell photoelectron gama                         *
C.    *    Beta  - L2 shell photoelectron beta                         *
C.    *    XM    - ratio of the electron mass to the photon energy     *
C.    *                                                                *
C.    *    ==>CALLED BY : G3PHOT                                       *
C.    *       AUTHOR    : J. Chwastowski                               *
C.    *                                                                *
C.    ******************************************************************
C.
#if !defined(CERNLIB_SINGLE)
      DOUBLE PRECISION G,G2,G3,B,P1,P2,P3
#endif
      G = GAMA
      G2 = G*G
      G3 = G2*G
      B  = BETA
      P1 = (9*G3-5*G2+24*G-16)
      P2 = -(G2+3*G-8)
      P3 = 1
      IF(B.GT.0.0001) P3 = LOG(G*(1+B))/(B*G)
      G3AVRL2 = (P1+P2*P3)*B*G*XM**5
      END
