* $Id: dzdwrt.F,v 1.1 2000/01/06 01:01:10 nevski Exp $
* $Log: dzdwrt.F,v $
* Revision 1.1  2000/01/06 01:01:10  nevski
* dzdoc for agi
*

      SUBROUTINE DZDWRT (CTEMP,N)
      CHARACTER  CTEMP*8
        if (N.ge.1000 000 .or. N.gt.1.and.mod(N,1000).eq.0) then
           WRITE(CTEMP,'(I7,1HK)') ((N-1)/1000+1)
        else
           WRITE(CTEMP,'(I8)') N
        endif
      end

