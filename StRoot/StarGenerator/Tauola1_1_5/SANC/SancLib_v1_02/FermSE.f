*      complex*16 function swwfer(thmu2,s)
      complex*16 function swwfer(s)
*----------------------------------------
      implicit none!
      include "s2n_declare.h"
*-------------------
      integer*4 i
      real*8 cf1(12)
      complex*16 bffsdu,b1fsud,b1fsdu,b0fsdu
      complex*16 mu2,md2
      data cf1/6*1d0,6*3d0/
*-
      swwfer = dcmplx(0d0,0d0)
      do 1 i=1,6
         mu2 = dcmplx((rmf1(2*i-1))**2,-1d-30)
         md2 = dcmplx((rmf1(2*i)  )**2,-1d-30)
         bffsdu = bff(-s,thmu2,md2,mu2)
         b0fsdu = b0f(-s,thmu2,md2,mu2)
         if(s.ne.0d0) then
            b1fsdu = -1d0/2/s*
     &           (md2*(log(dreal(md2)/thmu2)-1d0)
     &           -mu2*(log(dreal(mu2)/thmu2)-1d0)
     &           +(md2-mu2+s)*b0fsdu)
            b1fsud = -1d0/2/s*
     &           (mu2*(log(dreal(mu2)/thmu2)-1d0)
     &           -md2*(log(dreal(md2)/thmu2)-1d0)
     &           +(mu2-md2+s)*b0fsdu)
         else
            b1fsdu = -1d0/2*b0f(0d0,thmu2,md2,mu2)
     &           +1d0/2*(md2-mu2)*b0p(0d0,md2,mu2)
            b1fsud = -1d0/2*b0f(0d0,thmu2,mu2,md2)
     &           +1d0/2*(mu2-md2)*b0p(0d0,mu2,md2)
         endif
         swwfer = swwfer - s*cf1(2*i)*bffsdu
     &        +cf1(2*i-1)*mu2*b1fsdu+cf1(2*i)*md2*b1fsud
 1    continue
      end

*      complex*16 function szzfer(thmu2,s,stw2)
      complex*16 function szzfer(s)
*---------------------------------------------
      implicit none!
      include "s2n_declare.h"
*-------------------
      integer*4 i
      real*8 cf1(12),rmf2
      real*8 af,af2,vf,vf2
      complex*16 bffsff,b0fsff,mf2
*-
      data cf1/6*1d0,6*3d0/
*-
      szzfer = dcmplx(0d0,0d0)
      do 1 i=1,12
         rmf2 = (rmf1(i))**2
         mf2  = dcmplx(rmf2,-1d-30)
         bffsff = bff(-s,thmu2,mf2,mf2)
         b0fsff = b0f(-s,thmu2,mf2,mf2)
         af  = i3f(i)
         vf  = i3f(i)-2d0*qf(i)*stw2
         af2 = af**2
         vf2 = vf**2
         szzfer = szzfer+cf1(i)*(-(vf2+af2)*s*bffsff-2d0*af2*rmf2*b0fsff)
 1    continue
      end

*      complex*16 function pggfer(thmu2,s)
      complex*16 function pggfer(s)
*----------------------------------------
      implicit none!
      include "s2n_declare.h"
*-------------------
      integer*4 i
      real*8 cf1(12),rmf2
      complex*16 bffsff,mf2
*-
      data cf1/6*1d0,6*3d0/
*-
      pggfer = dcmplx(0d0,0d0)
      do 1 i=1,12
         rmf2   = (rmf1(i))**2
         mf2    = dcmplx(rmf2,-1d-30)
         bffsff = bff(-s,thmu2,mf2,mf2)
         pggfer = pggfer+4d0*cf1(i)*qf(i)**2*bffsff
 1    continue
      end
