* $Id: dzepsb.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzepsb.F,v $
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
      SUBROUTINE DZEPSB(IX1,IY1,IX2,IY2,LUOUT)
      CHARACTER*20 CCINT
      CALL DZDPLN(LUOUT,' n',-2)
      WRITE(CCINT,10)IX1,IY1,' m'
      CALL DZDPLN(LUOUT,CCINT,-1)
      WRITE(CCINT,10)IX2,IY1,' d'
      CALL DZDPLN(LUOUT,CCINT,-1)
      WRITE(CCINT,10)IX2,IY2,' d'
      CALL DZDPLN(LUOUT,CCINT,-1)
      WRITE(CCINT,10)IX1,IY2,' d'
      CALL DZDPLN(LUOUT,CCINT,-1)
      WRITE(CCINT,10)IX1,IY1,' d'
      CALL DZDPLN(LUOUT,CCINT,-1)
      CALL DZDPLN(LUOUT,' s',0)
      WRITE(LUOUT,*)' s'
10    FORMAT(2I8,A)
      END
****************************************************************
