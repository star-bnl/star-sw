* $Id: tpt_fit_sz.F,v 1.2 1998/01/27 00:35:46 fisyak Exp $
* $Log: tpt_fit_sz.F,v $
* Revision 1.2  1998/01/27 00:35:46  fisyak
* Split sources
*
      integer function tpt_fit_sz(x0,y0,at,bt,xlast,ylast,iflag,
     >                 x,y,z, az,bz,stot)
      implicit none
#include "PAM.inc"
      real    x0, y0, xlast, ylast, at, bt, az, bz, x, y, z
      integer iflag
      real    sr, sz, stot, srr, s1, srz, rc
      real    da, ds
      if(iflag.eq.0) then
        sr =0
        sz =0
        srz=0
        srr=0
        s1=0
        stot=0
      endif
      if (iflag.eq.1) then
        rc = 0.5*sqrt((at**2+1.0)/bt**2)
        da=sqrt((xlast-x)**2+(ylast-y)**2)*0.5
        ds=2.0*rc*asin(da/rc)
        stot = stot+ds
        sr=sr+stot
        sz=sz+z
        srz=srz+stot*z
        srr=srr+stot**2
        s1=s1+1
      endif
      if(s1.gt.1.0) then
        bz=(sz*srr-sr*srz)/(s1*srr-sr**2)
        az=(s1*srz-sr*sz)/(s1*srr-sr**2)
      endif
      tpt_fit_sz=STAFCV_OK
      end
