*
* $Id: gbfsig.F,v 1.1.1.2 2009/02/01 17:10:30 fisyak Exp $
*
* $Log: gbfsig.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:30  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:33  fisyak
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
      FUNCTION G3BFSIG(T,C)
C.    ******************************************************************
C.    *                                                                *
C.    *  It calculates a weight factor for the positron discrete       *
C.    *       (hard) bremsstrahlung cross section                      *
C.    *                                                                *
C.    *  Input:T kinetic energy (GeV)                                  *
C.    *        C cut for bremsstrahlung (BCUTE)                        *
C.    *        ETA ratio of positron to electron brems cross section   *
C.    *  Output: weight factor                                         *
C.    *                                                                *
C.    *  ===>  positron sigma = Fsig * electron sigma  <===            *
C.    *                                                                *
C.    *  Author   L. Urban                                             *
C.    *                                                                *
C.    ******************************************************************
C.
      ETA = G3CBREM(T)
      IF(ETA.LT.0.0001) THEN
         F=1.E-10
      ELSEIF(ETA.GT.0.9999) THEN
         F=1.
      ELSE
C
         E0=C/T
C
         IF(E0.GE.1.) THEN
            F=1.E-10
         ELSE
            ALFA=1./ETA
            E1=1.-E0
            F=ETA*E1**ALFA/E1
         ENDIF
      ENDIF
C
      G3BFSIG=F
      END
