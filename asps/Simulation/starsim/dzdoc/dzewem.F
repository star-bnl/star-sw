* $Id: dzewem.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzewem.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:25  nevski
*  first working release
*
* Revision 1.2  2000/01/06 23:57:24  nevski
* cvs header record corrected
*
* Revision 1.1  2000/01/06 01:01:12  nevski
* dzdoc for agi
*
      SUBROUTINE DZEWEM(LUNSGM)
      WRITE(LUNSGM,'(A)')'</BODY>'
      WRITE(LUNSGM,'(A)')'<BACKM>'
      WRITE(LUNSGM,'(A)')'<INDEX>'
      WRITE(LUNSGM,'(A)')'</BACKM>'
      WRITE(LUNSGM,'(A)')'</GDOC>'
      END

