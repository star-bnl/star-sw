* $Id: tpt_fit_uv.F,v 1.4 2000/11/14 22:29:08 genevb Exp $
* $Log: tpt_fit_uv.F,v $
* Revision 1.4  2000/11/14 22:29:08  genevb
* Switched from float to double in many places
*
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
      real    x0, y0, x, y
      real*8  at, bt
      real*8  su, sv, suv, suu, s1
      real*8  u, v, rr

      if(iflag.eq.0) then
C        Initialise track parameters
         su=0.0
         sv=0.0
         suu=0.0
         suv=0.0
         s1=0.0
      elseif(iflag.eq.1) then
         rr=(dble(x)**2-dble(x0)**2)+(dble(y)**2-dble(y0)**2)
         u= dble(x-x0)/rr
         v=-dble(y-y0)/rr
         sv=sv+v
         su=su+u
         suv=suv+u*v
         suu=suu+u**2
         s1=s1+1.0
         if(s1.gt.1.0) then
            rr = s1*suu-su**2
            at=(s1*suv-su*sv)/rr
            bt=(sv*suu-su*suv)/rr
         endif 
      endif
      tpt_fit_uv=STAFCV_OK
      end






