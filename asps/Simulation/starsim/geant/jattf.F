#include "geant321/pilot.h"
*CMZ :          18/11/98  14.45.41  by  Pavel Nevski
*-- Author :    Pavel Nevski   28/10/98
      FUNCTION   JATTF(JJ)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       calculates attribute position in Volume bank             *
C.    *    ==>Called by : <USER>                                       *
C.    *                                                                *
C.    ******************************************************************
C.
#include "commons/typing.inc"
#include "geant321/gcbank.inc"
      Integer  JATTF,Jj
      JATTF  =  Jj+int(Q(Jj+5))+6
      end

