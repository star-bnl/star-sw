*
* $Id: erbcer.F,v 1.2 2009/05/14 19:56:59 fisyak Exp $
*
* $Log: erbcer.F,v $
* Revision 1.2  2009/05/14 19:56:59  fisyak
* Flip sign for low triangular matrix
*
* Revision 1.1.1.3  2009/02/18 20:33:04  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:35  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:15  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 15:37:34  mclareni
* Add geane321 source directories
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.49  by  S.Giani
*-- Author :
      SUBROUTINE ERBCER (E)
*
C.    ******************************************************************
C.    *                                                                *
C.    *       After the back-tracking transforms the error matrix      *
C.    *                  back to the original                          *
C.    *                  particle direction                            *
C.    *                                                                *
C.    *       Arguments                                                *
C.    *           E        - Error Matrix (Triangular)                 *
C.    *                                                                *
C.    *    ==>Called by : ERPEND                                       *
C.    *       Author    E.Nagy  *********                              *
C.    *                                                                *
C.    ******************************************************************
*
         DIMENSION E(15)
*
      J = 0
      DO 10 I = 1, 5
         DO 5 K = 1, I
            J = J + 1
            IF (I.EQ.2) E(J) = -E(J)
            IF (K.EQ.2) E(J) = -E(J)
            IF (I.EQ.4) E(J) = -E(J)
            IF (K.EQ.4) E(J) = -E(J)
    5    CONTINUE
   10 CONTINUE
*                                                           END ERBCER
      END
