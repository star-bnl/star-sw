*
* $Id: versqq.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: versqq.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.4  2002/02/22 15:20:20  nevski
* move pointers to 3.0.0
*
* Revision 1.3  2001/10/28 17:02:45  nevski
* decode version
*
* Revision 1.2  2001/10/28 15:11:37  nevski
* my first working version
*
* Revision 1.1  2001/06/14 23:57:56  nevski
* emulate cmz identification
*
*
*KEEP,VERSQQ.
      CVERSQQ = CPP_TITLE_CH//'-'//CPP_VERS
      IVERSQ  = 0
      do i=1,LENOCC(CVERSQQ)
         j=ICHAR(CVERSQQ(i:i))
         if (48.le.j .and. j.lt.58) then
            IVERSQ=IVERSQ*10+(j-48)
            if (IVERSQ.eq.0) VERSQQ=CVERSQQ(i:)
         endif
      enddo

