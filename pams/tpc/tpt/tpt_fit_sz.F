* $Id: tpt_fit_sz.F,v 1.3 2000/11/14 22:29:07 genevb Exp $
* $Log: tpt_fit_sz.F,v $
* Revision 1.3  2000/11/14 22:29:07  genevb
* Switched from float to double in many places
*
* Revision 1.2  1998/01/27 00:35:46  fisyak
* Split sources
*
      integer function tpt_fit_sz(x0,y0,at,bt,xlast,ylast,iflag,
     >                 x,y,z, az,bz,stot)
      implicit none
#include "PAM.inc"
      real    x0, y0, xlast, ylast, x, y, z
      real*8  at, bt, az, bz, stot
      integer iflag
      real*8  sr, sz, srr, s1, srz, rc, sdenom
      real*8  da, ds
      if(iflag.eq.0) then
        sr =0.0
        sz =0.0
        srz=0.0
        srr=0.0
        s1=0.0
        stot=0.0
      elseif(iflag.eq.1) then
        rc = 0.5*dsqrt((at**2+1.0)/bt**2)
        da=dsqrt(dble(xlast-x)**2+dble(ylast-y)**2)*0.5
        ds=2.0*rc*dasin(da/rc)
        stot = stot+ds
        sr=sr+stot
        sz=sz+z
        srz=srz+stot*z
        srr=srr+stot**2
        s1=s1+1.0
        if(s1.gt.1.0) then
          sdenom = s1*srr-sr**2
          bz=(sz*srr-sr*srz)/sdenom
          az=(s1*srz-sr*sz)/sdenom
        endif
      endif
      tpt_fit_sz=STAFCV_OK
      end
