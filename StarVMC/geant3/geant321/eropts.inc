*
* $Id: eropts.inc,v 1.4 2011/02/11 15:51:15 fisyak Exp $
*
* $Log: eropts.inc,v $
* Revision 1.4  2011/02/11 15:51:15  fisyak
* Merge with latest updates
*
* Revision 1.3  2009/10/01 18:50:21  fisyak
* Add option Mix to geane
*
* Revision 1.2  2009/02/25 00:08:14  fisyak
* Clean up mess created after merging
*
* Revision 1.1.1.1  2008/12/10 20:47:09  fisyak
* Merge with macos version
*
* Revision 1.2  2005/06/09 00:09:25  fisyak
* comply with STAR
*
* Revision 1.2  2004/03/01 17:26:25  fisyak
* Get rid of staf
*
* Revision 1.1  1999/12/07 15:44:29  fisyak
* Add geane, new TGeant3 from Alice
*
* Revision 1.2  1996/07/25 15:01:02  ravndal
* CMZ to CVS conversion artefact removed
*
* Revision 1.1  1996/03/06 15:32:33  mclareni
* Add geane321 includes
*
*
#ifndef CERNLIB_ERPRCDES_EROPTS_INC
#define CERNLIB_ERPRCDES_EROPTS_INC
*
*      Sequence for conditions to stop tracking
*
* eropts.inc
*
      CHARACTER*12   CHOPTI
      LOGICAL         LEEXAC, LELENG, LEONLY, LEPLAN, LEPOIN, LEVOLU, 
     +                   LEVMIX, LMAXVO, LECONT
      REAL             ERPLI, ERPLO, ERLENG, RMIN, RMAX, ZMAX
      INTEGER         NAMEER, NUMVER, IOVLER
      COMMON /EROPTS/ ERPLI(3,2), ERPLO(3,4,MXPRED), ERLENG(MXPRED),
     +                NAMEER(MXPRED), NUMVER(MXPRED), IOVLER(MXPRED),
     +                LEEXAC, LELENG, LEONLY, LEPLAN, LEPOIN, LEVOLU, 
     +                LEVMIX, LMAXVO, LECONT
     +                RMIN, RMAX, ZMAX
      COMMON /EROPTC/CHOPTI
*
* eropts.inc
*
#if defined(CERNLIB_INCLUDE)
#include "geant231/incbeg.inc"
     +    EROPTS
#include "geant321/incend.inc"
#endif

#endif
