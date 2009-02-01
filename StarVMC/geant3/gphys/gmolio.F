*
* $Id: gmolio.F,v 1.1.1.2 2009/02/01 17:10:32 fisyak Exp $
*
* $Log: gmolio.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:32  fisyak
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
      SUBROUTINE G3MOLIO(AC,ZC,WMAT,NLM,DENS,BETA2,CHARG2,OMC)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *     Initializes material constants OMC,CHC for Moliere         *
C.    *     scattering.                                                *
C.    *                                                                *
C.    * WMAT=proportion by weight of NLM  individual constituents of   *
C.    *      charge ZC and atomic weight AC for the mixture/compound   *
C.    * DENS=density in gm/cm**3                                       *
C.    *                                                                *
C.    *    ==>Called by : G3MULOF                                      *
C.    *         Author M.S. Dixit NRCC Ottawa    *********             *
C.    *                                                                *
C.    ******************************************************************
C.
C     ZETA=factor for scattering by atomic electrons
C
      DIMENSION WMAT(*),AC(*),ZC(*)
      PARAMETER (ZETA=1.)
*
* *** This is equal to 3.34/137.036**2
*
      PARAMETER (CONST1 = 1.778595E-4)
*
*     ------------------------------------------------------------------
*
      ZS     = 0.
      ZE     = 0.
      ZX     = 0.
      CONST  = CONST1*CHARG2/BETA2
      DO 10 I=1,NLM
         TMP   = (WMAT(I)/AC(I))*ZC(I)*(ZC(I)+ZETA)
         ZS    = ZS+TMP
         ZE    = ZE-0.66666*TMP*LOG(ZC(I))
         ZX    = ZX+TMP*LOG(1.+CONST*ZC(I)**2)
  10  CONTINUE
      OMC    = 6702.33*ZS*DENS*EXP((ZE-ZX)/ZS) * CHARG2
*
      END
 
