* $Id: tpt_fit_uv.F,v 1.3 1999/10/26 18:16:49 liq Exp $
* $Log: tpt_fit_uv.F,v $
* Revision 1.3  1999/10/26 18:16:49  liq
* change rr defination from (x-x0)**2+(y-y0)**2 to (x**2-x0**2)+(y**2-y0**2), fixed the bug
*
* Revision 1.2  1998/01/27 00:35:47  fisyak
* Split sources
*
      integer function tpt_fit_uv(x0,y0,iflag,x,y,at,bt)
      implicit none
#include "PAM.inc"
      integer iflag
      real    x0, y0, x, y, at, bt
      real    su, sv, suv, suu, s1
      real    u, v, rr

      if(iflag.eq.0) then
C        Initialise track parameters
         su=0
         sv=0
         suu=0
         suv=0
         s1=0
      endif
      if(iflag.eq.1) then
         rr=(x**2-x0**2)+(y**2-y0**2)
         u= (x-x0)/rr
         v=-(y-y0)/rr
         sv=sv+v
         su=su+u
         s1=s1+1
         suv=suv+u*v
         suu=suu+u**2
      endif 
      if(s1.gt.1.0) then
         at=(s1*suv-su*sv)/(s1*suu-su**2)
         bt=(sv*suu-su*suv)/(s1*suu-su**2)
      endif
      tpt_fit_uv=STAFCV_OK
      end






