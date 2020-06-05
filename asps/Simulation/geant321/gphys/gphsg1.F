*
* $Id: gphsg1.F,v 1.2 2020/06/04 22:27:12 jwebb Exp $
*
* $Log: gphsg1.F,v $
* Revision 1.2  2020/06/04 22:27:12  jwebb
* Make array lookup save from roundoff error
*
* Revision 1.1.1.1  2004/01/15 00:12:14  potekhin
*
*
* Revision 1.1.1.1  1997/11/03 15:30:40  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:29  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.22  by  S.Giani
*-- Author :
      FUNCTION GPHSG1(E)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       FUNCTION TO COMPUTE THE PHOTOEFFECT TOTAL CROSS-SECTION  *
C.    *       AS A FUNCTION OF E                                       *
C.    *                                                                *
C.    *    ==>CALLED BY : GPHOTI                                       *
C.    *                                                                *
C.    *    (1) Sandia parametrizsation is used;                        *
C.    *    (2) GPHSG1 in cm**2/g.                                      *
C.    *    (3) E in GeV                                                *
C.    *                                                                *
C.    *     J. Chwastowski 17.11.92                                    *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gconsp.inc"
#include "geant321/gc10ev.inc"
#if !defined(CERNLIB_SINGLE)
      DOUBLE PRECISION EINV,ECUR,ONE
#endif
      PARAMETER (ONE=1)
C.
C.    ------------------------------------------------------------------
C.
      RES = 0.
      IF(E.LT.G10EV) GO TO 20
C Use Sandia data
      JPHXS = LQ(JPHOT-1)
      IPOINT =       Q(JPHXS+1)*3.0+2.0 
      IMAX = Q(JPHXS+IPOINT)
      IPOINT = IPOINT+1
      ECUR = E*1.E6
      IF(ECUR.LT.Q(JPHXS+IPOINT)) GO TO 20
      EINV = ONE/ECUR
      DO 10 I = 2,IMAX
         IPOINT = IPOINT+5
         IF(ECUR.LT.Q(JPHXS+IPOINT)) THEN
            J = IPOINT+1
            RES = EINV*(Q(JPHXS+J)+
     >            EINV*(Q(JPHXS+J+1)+
     >            EINV*(Q(JPHXS+J+2)+
     >            EINV*Q(JPHXS+J+3))))
            GO TO 20
         ENDIF
   10 CONTINUE
C This value is in cm**2/g
   20 GPHSG1 = MAX(RES,0.)
C
      END
