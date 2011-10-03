*
* $Id: gfhsta.F,v 1.1.1.1 2004/01/15 00:12:03 potekhin Exp $
*
* $Log: gfhsta.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:03  potekhin
*
* Revision 1.2  1998/02/10 13:04:45  atgeant3
* *** empty log message ***
*
* Revision 1.1.1.1  1997/11/03 15:29:26  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.19  by  S.Giani
*-- Author :
      SUBROUTINE GFHSTA
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Fills histograms statistics                              *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GUOUT                                *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gclist.inc"
      COMMON/QUEST/IQUEST(100)
C.
C.    ------------------------------------------------------------------
C.
      IF(NHSTA.GT.0) THEN
         CALL GLOOK('TIME',LHSTA,NHSTA,ID)
         IF(ID.NE.0)THEN
            CALL TIMED(TEVENT)
            CALL HFILL(-ID,TEVENT,0.,1.)
         ENDIF
*
         CALL GLOOK('SIZE',LHSTA,NHSTA,ID)
         IF(ID.NE.0)THEN
            NL=NZLEFT(IXDIV,0) ! (Unused argument is integer)
            CALL HFILL(-ID,IQUEST(11)+0.5,0.,1.)
         ENDIF
*
         CALL GLOOK('MULT',LHSTA,NHSTA,ID)
         IF(ID.NE.0)THEN
            CALL HFILL(-ID,NTMULT+0.5,0.,1.)
         ENDIF
*
         CALL GLOOK('NTRA',LHSTA,NHSTA,ID)
         IF(ID.NE.0)THEN
            CALL HFILL(-ID,NTRACK+0.5,0.,1.)
         ENDIF
*
         CALL GLOOK('STAK',LHSTA,NHSTA,ID)
         IF(ID.NE.0)THEN
            CALL HFILL(-ID,NSTMAX+0.5,0.,1.)
         ENDIF
*
      ENDIF
      END
