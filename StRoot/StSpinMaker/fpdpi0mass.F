      subroutine initfpdpi0(filename)
#include "fpd_ntdat.inc"
      character*100 filename
      integer ISTAT
      COMMON /PAWC/ hmem(2000000)
      CALL HLIMIT(1980000)         
      call book
      call hropen(1,'fpd',filename,'N',2048,ISTAT)
      print *, '>>> N-tuple file opened <<<'
      if(ISTAT.eq.0) then
         call hbnt(12,'BLOCK1',' ')
         call hbname(12,'BLOCK1',n4evt,
     $        'n4evt,npkpar,'//
     $        'fmss,ener,phi,eta,zgam,'//
     $        'smx1sm,smx1cn,smx1sg,'//
     $        'smx2sm,smx2cn,smx2sg,'//
     $        'smy1sm,smy1cn,smy1sg,'//
     $        'smy2sm,smy2cn,smy2sg,'//
     $        'smxq,smyq,'//
     $        'tower(12),'//
     $        'pren1(12),pren2(12),'//
     $        'south(16),top(16),bot(16),'//
     $        'tnsum,tssum,ttsum,tbsum,'//
     $        'smxsum,smysum,'//
     $        'time(7),zvertex,token:I,bunch:I,bunch48:I,trgwd:I,'//
     $        'nprim:I,lcppt:R,lcpeta:R,lcpphi:R,'//
     $        'bbcadc(32):I,bbctdc(32):I')
         print *, '>>> N-tuple BOOKED <<<'
      else
         print *, 'File open error'
         print *, '>>> N-tuple NOT BOOKED <<<'
      endif

      return
      end

      subroutine fpdpi0mass(adc,iin,rin,result,abbc,tbbc)
#include "fpd_pi0dat.inc"
#include "fpd_ntdat.inc"

      integer ierr, run
      integer n, t, b
 
      integer adc(256), iin(10), abbc(32), tbbc(32)
      real  rin(10), result(10)
      integer i, j
      integer sumSMD1,sumSMD2,sumN       
      integer nmax
      real zgam,SMXdif,SMYdif,SMDdif,finv
    
      n=1
      tnsum=0
      tssum=0
      ttsum=0
      tbsum=0
      smxsum=0
      smysum=0
      do i=61,120
         smxsum=smxsum+adc(i)
      enddo
      do i=121,220
         smysum=smysum+adc(i)
      enddo
      do i=1,12
         tnsum=tnsum+adc(i+48)         
         towen(i)=adc(i+48)
         prent1(i)=adc(i+220)
         prent2(i)=adc(i+236)
      enddo
      do i=1,16
         southnt(i)=adc(i+32)
         topnt(i)  =adc(i)
         botnt(i)  =adc(i+16)
         tssum     =tssum + adc(i+32)
         ttsum     =ttsum + adc(i)
         tbsum     =tbsum + adc(i+16)
      enddo
      do i=1,32
         bbcadc(i) = abbc(i)
         bbctdc(i) = tbbc(i)
*         write(*,*) 'BBC ',i,bbcadc(i),bbctdc(i)
      enddo
      token = iin(1)
      bunch = iin(2)
      trgwd = iin(3)
      nprim = iin(4)
      run   = iin(5)
      n4evt = iin(6)
      nttim(1) = float(iin(7))
      nttim(2) = float(iin(8))
      bunch48  = iin(9)
      tpczv  = rin(1)
      nttim(3) = rin(2)
      lcppt  = rin(3)
      lcpeta = rin(4)
      lcpphi = rin(5)
      nttim(4) = rin(6)
      nttim(5) = rin(7)
      nttim(6) = rin(8)
      nttim(7) = rin(9)
      if(smxsum.gt.100 .and. smysum.gt.100 .and. tnsum.gt.300) then
         call hgnt(10,n,ierr)
         call init
         write(6,*) 'Analyze event ',n
         call fillpi1(adc,run)      ! fill single event histograms
         call anasmd(tpczv, rin(2), run)   
         call fillpi2           ! fill cumulative histograms
         nmax=(nrmax-1)*4+ncmax
         zgam=(p1(4)-p2(4))/(p1(4)+p2(4))
         SMXdif=SMXp1cnt-SMXp2cnt
         SMYdif=SMYp1cnt-SMYp2cnt
         SMDdif=sqrt(SMXdif*SMXdif+SMYdif*SMYdif)
         if(finv2.gt.0) then
            finv=sqrt(finv2)
         else
            finv=-sqrt(-finv2)
         endif         
         result(1)=esum
         result(2)=finv
         result(3)=zgam
         result(4)=etapi
         result(5)=phipi
         result(6)=SMDdif
         result(7)=tpczv
         result(8)=rin(2)
      else
         npkpar=-1
         do i=1,10
            result(i)=0.0
         enddo
         call hfnt(12)
      endif

      return
      end

      subroutine finishfpdpi0()
      integer icycle

      call hcdir('//fpd',' ')
      call hrout(12,icycle,' ')
      call hrend('fpd')
      close(1)
*      call hdelet(12)
      print *,'Just wrote/closed ntuple file'

*      call hropen(10,'LUN10','fpdpi0.hist','N',1024,ISTAT)
*      call hrout(0,icycle,' ')
*      call hrend('LUN10')
      return
      end








