*
* $Id: dzdpsc.F,v 1.2 1998/04/16 16:11:38 fisyak Exp $
*
* $Log: dzdpsc.F,v $
* Revision 1.2  1998/04/16 16:11:38  fisyak
* put gstar in cvs
*
* Revision 1.1  1998/04/10 15:31:05  fisyak
* Move gstar into CVS
*
*
#include "packlib/zebra/zebra/pilot.h
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
 
