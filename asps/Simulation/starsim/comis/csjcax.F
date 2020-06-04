*
* $Id: csjcax.F,v 1.3 2020/06/04 23:05:02 perev Exp $
*
* $Log: csjcax.F,v $
* Revision 1.3  2020/06/04 23:05:02  perev
* add csguard call
*
* Revision 1.2  2009/01/18 23:51:38  perev
* BugFix. imitation of stack, non static variable
*
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
*
* Revision 1.3  2003/10/03 13:24:12  nevski
* make comis call re-entrant
*
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.01  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSJCAX(J,NP,M)
***------------------------------
      INTEGER CSINT1
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
         PARAMETER (KLK=45, KCALLS=87, KEXIT=113)
         INTEGER M(2)
*PN, 03.10.03: make it re-entrant
         integer nStk/0/,iStk(100)
         save iStk

       I=MHLOC(64) 
       nStk = nStk+1
       if (nStk .gt. 100) call abort
       iStk(nStk) = I
       K=I+2*NP
       IQ(K)=KCALLS
       IQ(K+1)=J
       IQ(K+2)=NP
       I3=K+3
       I2=I
       DO 1 K=1,NP
         IQ(I3)=1
         IQ(I2)=KLK
         IQ(I2+1)=M(K)
         I2=I2+2
   1  I3=I3+1
      IQ(I3)=KEXIT
      INT=CSINT1(I)
**      IF(INT.LE.0)THEN
**         CONTINUE
**      ENDIF
       I = iStk(nStk)
       nStk = nStk-1
      CALL MHFREE(I)
      call csguard()
      END
