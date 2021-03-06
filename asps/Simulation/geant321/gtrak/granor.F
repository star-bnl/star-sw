*
* $Id: granor.F,v 1.1.1.1 2004/01/15 00:12:17 potekhin Exp $
*
* $Log: granor.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:17  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:47  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:42  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.23  by  S.Giani
*-- Author :
      SUBROUTINE GRANOR(A,B)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       To generate 2 numbers A and B following a NORMAL         *
C.    *       distribution (mean=0 sigma=1.)                           *
C.    *         Copy of the CERN Library routine RANNOR                *
C.    *                                                                *
C.    *    ==>Called by : <USER>, many GEANT routines                  *
C.    *       Author    F.Carminati *********                          *
C.    *                                                                *
C.    ******************************************************************
C.
      DIMENSION RNDM(2)
*
      CALL GRNDM(RNDM,2)
      Y=RNDM(1)
      Z=RNDM(2)
      X=6.283185*Z
      A1=SQRT (-2.0*LOG(Y))
      A=A1*SIN (X)
      B=A1*COS (X)
      RETURN
      END
 
