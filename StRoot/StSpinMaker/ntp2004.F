      subroutine initntp2004(filename)
      character*100 filename   
      integer ISTAT, ew, nstb, ch, ih
      character*3 cEW /'EW'/
      character*5 cNSTB /'NSTB'/
      character*3 cEWNSTB
      character*40 hname
      COMMON /PAWC/ hmem(2000000)
#include "ntp2004.inc"
      CALL HLIMIT(1980000)         

      call hropen(1,'NTP',filename,'N',2048,ISTAT)
      write(*,*) '>>> N-tuple file opened <<<'
      if (ISTAT.eq.0) then
         call hbnt(111,'BLOCK1',' ')
         call hbname(111,'BLOCK1',event,ntpdef)
         
         do ew=1,2
            do nstb=1,4
               write(cEWNSTB,*) cEW(ew:ew)//cNSTB(nstb:nstb)
               do ch=1,4                  
                  ih = 1000*ew + 100*nstb + 10*ch
                  write(hname,*) cEWNSTB,'add=',ch,' L0-L1'
                  call hbook2(ih,hname,21,-10.5,10.5,11,0.0,11.0,0.0)
               enddo
               ih = 1000*ew + 100*nstb
               write(hname,*) cEWNSTB,'L0-L2'
               call hbook2(ih,hname,21,-10.5,10.5,11,0.0,11.0,0.0)
               ih = 1000*ew + 100*nstb + 1
               write(hname,*) cEWNSTB,'L1-L2'
               call hbook1(ih,hname,21,-10.5,10.5,0.0)
            enddo
         enddo

         write(*,*) '>>> N-tuple is BOOKED! <<<'
      else
         write(*,*) 'File open error'
         write(*,*) '>>> N-tuple is NOT BOOKED <<<'
      endif
      return
      end
      
      subroutine fillntp2004(iin,rin,iout,rout)      
      integer   iin(100),iout(100)
      real      rin(100),rout(100)
      integer i, run
#include "ntp2004.inc"

      run=iin(1)
      call hfnt(111)
      return
      end

      subroutine finishntp2004()
      integer icycle
      call hcdir('//NTP',' ')
      call hrout(0,icycle,' ')
      call hrend('NTP')
      close(1)
      write(*,*) 'Just wrote/closed ntuple file'
      return
      end




