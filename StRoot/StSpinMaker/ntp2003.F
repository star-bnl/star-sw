      subroutine initntp2003(filename)
      character*100 filename   
      integer ISTAT
      COMMON /PAWC/ hmem(2000000)
      include 'StSpinMaker/ntp2003.inc'
      CALL HLIMIT(1980000)         

      call hropen(1,'NTP',filename,'N',2048,ISTAT)
      write(*,*) '>>> N-tuple file opened <<<'
      if (ISTAT.eq.0) then
         call hbnt(111,'BLOCK1',' ')
         call hbname(111,'BLOCK1',event,ntpdef)
         write(*,*) '>>> N-tuple is BOOKED! <<<'
      else
         write(*,*) 'File open error'
         write(*,*) '>>> N-tuple is NOT BOOKED <<<'
      endif
      return
      end
      
      subroutine fillntp2003(iin,rin,iout,rout)      
      integer   iin(100),iout(100)
      real      rin(100),rout(100)
      integer i, run
      include "StSpinMaker/ntp2003.inc"

      run=iin(1)
      call hfnt(111)
      return
      end

      subroutine finishntp2003()
      integer icycle
      call hcdir('//NTP',' ')
      call hrout(111,icycle,' ')
      call hrend('NTP')
      close(1)
      write(*,*) 'Just wrote/closed ntuple file'
      return
      end








