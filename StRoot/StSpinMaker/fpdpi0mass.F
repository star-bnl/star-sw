      subroutine fpdpi0mass(adc,zvertex,result)
      implicit none
      integer adc(256)
      real  result(10), zvertex
      integer i, j
      real sum

      sum=0.0
      do i=1,256
         sum=sum+adc(i)
      enddo
      if(sum.gt.0) then
         do i=1,32 
            j=(i-1)*8
            write(*,*) adc(j+1),adc(j+2),adc(j+3),adc(j+4),
     +           adc(j+5),adc(j+6),adc(j+7),adc(j+8)
         enddo
      endif

      result(1)=sum
      result(2)=zvertex
      result(3)=2.6
      result(4)=3.7
      result(5)=4.8
      return
      end

      
