*
* $Id: gmoli.F,v 1.1.1.2 2009/02/01 17:10:31 fisyak Exp $
*
* $Log: gmoli.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:31  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:40  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:54  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:25  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:41  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:20  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:27  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.22  by  S.Giani
*-- Author :
      SUBROUTINE G3MOLI (AC,ZC,WMAT,NLM,DENS,OMC,CHC)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *     Initializes material constants OMC,CHC for Moliere         *
C.    *     scattering.                                                *
C.    *                                                                *
C.    * WMAT=proportion by weight of NLM  individual constituents of   *
C.    *      charge ZC and atomic weight AC for the mixture/compound   *
C.    * DENS=density in gm/cm**3                                       *
C.    * Warning : the beta and charge dependance of ZX is omitted      *
C.    *                                                                *
C.    *    ==>Called by : G3PROBI                                      *
C.    *         Author M.S. Dixit NRCC Ottawa    *********             *
C.    *                                                                *
C.    ******************************************************************
C.
C     ZETA=factor for scattering by atomic electrons
C
      DIMENSION WMAT(*),AC(*),ZC(*)
      PARAMETER (ZETA=1)
*
*     ------------------------------------------------------------------
*
      ZS=0.
      ZE=0.
      ZX=0.
      DO 15 I=1,NLM
         TMP=(WMAT(I)/AC(I))*ZC(I)*(ZC(I)+ZETA)
         ZS=ZS+TMP
         ZE=ZE-0.66666*TMP*LOG(ZC(I))
         ZX=ZX+TMP*LOG(1.+3.34*(ZC(I)/137.036)**2)
15    CONTINUE
      CHC=0.39612E-3*SQRT(DENS*ZS)
      OMC=6702.33*ZS*DENS*EXP((ZE-ZX)/ZS)
*
      END
