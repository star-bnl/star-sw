*
* $Id: gxsi.F,v 1.1.1.1 2004/01/15 00:12:04 potekhin Exp $
*
* $Log: gxsi.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:04  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:38  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:18  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.20  by  S.Giani
*-- Author :
      FUNCTION GXSI (Z)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  COMPUTE SCREENING FACTOR   FOR PAIR PRODUCTION AND BREM       *
C.    *  REFERENCE : EGS MANUAL SLAC 210 - UC32 - JUNE 78              *
C.    *                        FORMULA 2.7.22                          *
C.    *                                                                *
C.    *    ==>Called by : GSMIXT                                       *
C.    *       Author    M.Maire *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
      DATA AL183  , AL1440 / 5.20948 , 7.27239 /
      ALZ  = LOG(Z)/3.
      GXSI  = (AL1440 - 2*ALZ) / (AL183 - ALZ - GFCOUL(Z))
      END
