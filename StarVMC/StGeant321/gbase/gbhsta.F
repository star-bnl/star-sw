*
* $Id: gbhsta.F,v 1.1.1.1 2004/07/17 20:02:42 perev Exp $
*
* $Log: gbhsta.F,v $
* Revision 1.1.1.1  2004/07/17 20:02:42  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.2  2003/11/28 11:23:55  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:24  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:37  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:08  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.19  by  S.Giani
*-- Author :
      SUBROUTINE G3BHSTA
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Books histograms statistics                              *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GUOUT                                *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gclist.inc"
C.
C.    ------------------------------------------------------------------
C.
      IF(NHSTA.GT.0)THEN
         CALL GLOOK('TIME',LHSTA,NHSTA,ID)
         IF(ID.NE.0) CALL HBOOK1(-ID,'Time per event$',100,0.,0.,0.)
*
         CALL GLOOK('SIZE',LHSTA,NHSTA,ID)
         IF(ID.NE.0) CALL HBOOK1(-ID,'Space used in IXDIV per event$',
     +   100,0.,0.,0.)
*
         CALL GLOOK('MULT',LHSTA,NHSTA,ID)
         IF(ID.NE.0) CALL HBOOK1(-ID,'Total number of tracks per event$'
     +   ,100,0.,0.,0.)
*
         CALL GLOOK('NTRA',LHSTA,NHSTA,ID)
         IF(ID.NE.0) CALL HBOOK1(-ID,'Long life tracks per event$',100,
     +   0.,0.,0.)
*
         CALL GLOOK('STAK',LHSTA,NHSTA,ID)
         IF(ID.NE.0) CALL HBOOK1(-ID,'Maximum stack size per event$',
     +   100,0.,0.,0.)
*
      ENDIF
      END
