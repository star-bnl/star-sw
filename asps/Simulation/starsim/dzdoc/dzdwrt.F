* $Id: dzdwrt.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzdwrt.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:23  nevski
*  first working release
*
* Revision 1.2  2000/01/07 00:05:29  nevski
* cvs header record corrected
*

      SUBROUTINE DZDWRT (CTEMP,N)
      CHARACTER  CTEMP*8
        if (N.ge.1000 000 .or. N.gt.1.and.mod(N,1000).eq.0) then
           WRITE(CTEMP,'(I7,1HK)') ((N-1)/1000+1)
        else
           WRITE(CTEMP,'(I8)') N
        endif
      end

