* $Id: dzenpg.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzenpg.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:24  nevski
*  first working release
*
* Revision 1.2  2000/01/06 23:57:24  nevski
* cvs header record corrected
*
* Revision 1.1  2000/01/06 01:01:11  nevski
* dzdoc for agi
*
      SUBROUTINE DZENPG(IPAGE)
#include "dzedit/dzesgpkeep.inc"
      CHARACTER*28 CCINT
      IPAGE = IPAGE + 1
      WRITE(CCINT,'(A,2I5)')'%%Page:',IPAGE,IPAGE
      CALL DZDPLN(LUOUT,'/saveobj save def',-3)
      CALL DZDPLN(LUOUT,CCINT,-3)
      WRITE(CCINT,'(2F8.5,A)')SCALE,SCALE,' scale'
      CALL DZDPLN(LUOUT,CCINT,-3)
      CALL DZDPLN(LUOUT,' [] 0 setdash',-1)
      CALL DZDPLN(LUOUT,' 1 setlinewidth',-1)
      CALL DZDPLN(LUOUT,'H 40 sf',-1)
      WRITE(CCINT,'(2I5,A,I5,A)')IXPAGE/2,MARGB -2*ISPACE,
     &                 ' 0 (-',IPAGE,'-) cs'
      CALL DZDPLN(LUOUT,CCINT,-3)
      WRITE(CCINT,'(A,I5,A)')'C ',ICSIZE,' sf'
      CALL DZDPLN(LUOUT,CCINT,-3)
      END
****************************************************************
