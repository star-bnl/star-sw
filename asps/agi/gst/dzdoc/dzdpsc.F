*
* $Id: dzdpsc.F,v 1.3 1998/04/16 17:00:28 fisyak Exp $
*
* $Log: dzdpsc.F,v $
* Revision 1.3  1998/04/16 17:00:28  fisyak
* 2nd pass with gstar
*
*
#include "dzdoc/pilot.h"
*CMZ :  1.30/00 14/02/96  21.04.12  by  Pavel Nevski
*-- Author :    Otto Schaile  14/02/96
******************************************************************************
*                                                                            *
      SUBROUTINE DZDPSC(ISTORE,LEV,CF)
*                                                                            *
* draw bank tree below LEV in store ISTORE in PostScript format into file CF *
******************************************************************************
      INTEGER LEV
      INTEGER LUNTRE, IRET
      PARAMETER (LUNTRE=65)
      CHARACTER*(*) CF
      CALL KUOPEN(LUNTRE,CF,'UNKNOWN',IRET)
      CALL IOPWK(2,LUNTRE,-111)
      CALL IDAWK(1)
      CALL IACWK(2)
      CALL RZCDIR('//RZDOC',' ')
      CALL DZDDTR(ISTORE,LEV,0,0,0,2,-111,'MPR')
      CALL IUWK(0,1)
      CALL IDAWK(2)
      CALL IACWK(1)
      CALL ICLWK(2)
      END
 
