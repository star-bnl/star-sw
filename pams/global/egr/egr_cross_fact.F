      INTEGER*4 FUNCTION EGR_CROSS_FACT(idet,xnew,ynew,xold,yold,cf,
     +                                 theta)
 
      IMPLICIT NONE
#include "tpt_const.inc"
C_______________________________________________________________________
      INTEGER idet
      REAL    xnew,ynew,xold,yold,cf,theta
      REAL    pi,xv,yv,alpha,isect
      PARAMETER (pi=3.141592653589)
C______________________________________________________________________
 
      if(cf.lt.0) then
         cf=1.0
      else
         isect = idet/100
         alpha = real(2*isect-1)*pi/12.0
         xv = xnew-xold
         yv = ynew-yold
         cf = (-sin(alpha)*xv+cos(alpha)*yv)/sqrt(xv**2+yv**2)
         if(abs(cf).gt.0.99) cf = sign(.99,cf)
         theta = acos(cf)
         theta = abs(cpi2-theta)
         cf = 1.0 -cf**2
      endif
      egr_cross_fact = 0
      end
