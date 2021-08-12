*
* $Id: lgkine.F,v 1.1.1.1 2004/01/15 00:12:03 potekhin Exp $
*
* $Log: lgkine.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:03  potekhin
*
* Revision 1.1  1998/02/10 13:04:48  atgeant3
* *** empty log message ***
*
*
#include "geant321/pilot.h"

      INTEGER FUNCTION LGKINE(JO,I)
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
* Name : LGKINE                                                        *
*                                                                      *
* Description :                                                        *
*        return Link to the t-th track in LgKINE and Link              *
*        to its mother bank (in JKINE linear chain) in J0              *
*                                                                      *
* Arguments   :                                                        *
*        [name]  ([in | out | in/out])      [description]              *
*           JO      out Link to Mother bank in JKINE linear chain      *
*            I      in  Track number                                   *
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
* Author : Pavel Nevski                       Date : 22/03/97          *
* Last modifications :                                                 *
*        Gilbert Poulard   Convert to Fortran 77     29/05/97          *
*                                                                      *
* Warnings this is a function which modifies its calling arguments     *
*          the returned, modified value is first in the list and the   *
*          input argument is second                                    *
*                                                                      *
*----------------------------------------------------------------------*
*
*.
      IMPLICIT NONE !  include "geant321/typing.inc"
#include "geant321/gcbank.inc"
C ----------- end CDE --------------------------------------------------
      INTEGER  I,J,JO,NF
*
      LGKINE=0
      J=JKINE
      JO=J
      NF=IQ(J+2)
C
C    Loop here
1011     IF(0.GE.J.OR.J.GE.NZEBRA) GO TO 1012
         JO=J
            IF (NF.EQ.0) THEN
C
               IF (I .GT.IQ(J+1)) THEN
               J=LQ(J)
            ELSE IF(I.LE.IQ(J+2)) THEN
               J=LQ(J+2)
            ELSE
               LGKINE=LQ(J-I+IQ(J+2))
               GO TO 1
            END IF
         ELSE
C
               IF (I.LE.IQ(J+2)) THEN
               J=LQ(J)
            ELSE
               LGKINE=LQ(J-I+IQ(J+2))
               GO TO 1
            END IF
         END IF
      GO TO 1011
1012  CONTINUE
C       Print *,' LgKINE error - track ',I,' not found'
1     CONTINUE
      END          ! LGKINE
