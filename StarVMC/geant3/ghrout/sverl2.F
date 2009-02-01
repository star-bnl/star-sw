*
* $Id: sverl2.F,v 1.1.1.2 2009/02/01 17:10:29 fisyak Exp $
*
* $Log: sverl2.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:29  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:23  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:37  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2004/02/24 15:50:30  brun
* From Peter Hristov:
* We had some problems with the Geant3 version during the tests for the
* physics data challenge. They are related to the fact that TRandom3 for
* sure doesn't generate 0, but it may return 1, so things like
* CALL GRANDOM(RNDM,1)
* X = -LOG(1-RNDM(1))
* may lead to floating point exceptions. So I have replaced most of such
* calls with
* X = -LOG(RNDM(1))
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:40  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:19  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:13  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.40  by  S.Giani
*-- Author :
      SUBROUTINE SVERL2(M,X)
      DIMENSION U(2)
      CALL GRNDM(U,2)
C PH      X=0.62666*LOG((1.+U(1))/(1.-U(1)))
      X=0.62666*LOG( (2.-U(1))/U(1) )
      IF(U(2).LT.0.5) X=-X
      X=M+X*SQRT(M*1.)
      RETURN
      END
