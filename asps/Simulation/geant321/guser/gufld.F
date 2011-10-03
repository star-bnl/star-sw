*
* $Id: gufld.F,v 1.1.1.1 2004/01/15 00:12:17 potekhin Exp $
*
* $Log: gufld.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:17  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:46  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.25  by  S.Giani
*-- Author :
      SUBROUTINE GUFLD(X,F)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine to compute the magnetic field F             *
C.    *       at space point X                                         *
C.    *                                                                *
C.    *    ==>Called by : GHELIX,GRKUTA                                *
C.    *                                                                *
C.    ******************************************************************
C.
      DIMENSION X(3),F(3)
C.
C.    ------------------------------------------------------------------
C.
      F(1)=0.
      F(2)=0.
      F(3)=0.
C
      END
