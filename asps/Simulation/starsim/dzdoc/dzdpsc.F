*
* $Id: dzdpsc.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: dzdpsc.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:23  nevski
*  first working release
*
* Revision 1.5  2000/01/07 00:05:29  nevski
* cvs header record corrected
*
* Revision 1.4  2000/01/06 01:01:09  nevski
* dzdoc for agi
*
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
*                 LUNSGM,LUNLOG,LUNMET,IWMETA,ITMETA,L,CHOPT
      CALL DZDDTR(0,0,0,2,-111,LEV,'MPR')
      CALL IUWK(0,1)
      CALL IDAWK(2)
      CALL IACWK(1)
      CALL ICLWK(2)
      END

