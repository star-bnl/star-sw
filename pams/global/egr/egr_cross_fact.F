C>-------------------------------------------------------------------
C
C EGR_CROSS_FACT - evaluates crossing angle and weighting 
C                  factor for error
C
C DESCRIPTION:
C The angle is evaluated from a dot prodact of a vector tangent 
C to the track and a vector parallel to the row.
C
C INPUT ARGUMENTS:
C  idet - sector**100 + row
C  xnew - current x
C  ynew - current y
C  xold - previous x
C  yold - previous y
C
C OUTPUT ARGUMENTS:
C  cf   - sin(crossing angle)**2
C  theta- crossing angle
C
C<--------------------------------------------------------------------
      INTEGER*4 FUNCTION EGR_CROSS_FACT(idet,xnew,ynew,xold,yold,cf,
     +                                 theta)
 
      IMPLICIT NONE
C#include "tpt_const.inc"
#include "math_constants.inc"
C_______________________________________________________________________
      INTEGER idet
      REAL    xnew,ynew,xold,yold,cf,theta
      REAL    vlocal(3),vglobal(3)
      integer tgc_global_to_local_p,isect,iret
C______________________________________________________________________
 
      if(cf.lt.0) then
         cf=1.0
      else
         isect = idet/100
*         alpha = real(2*isect-1)*C_PI/12.0
*         xv = xnew-xold
*         yv = ynew-yold
*         cf = (-sin(alpha)*xv+cos(alpha)*yv)/sqrt(xv**2+yv**2)
*         if(abs(cf).gt.0.99) cf = sign(.99,cf)
*         theta = acos(cf)
*         theta = abs(cpi2-theta)
*         cf = 1.0 -cf**2

         vglobal(1) = xnew-xold
         vglobal(2) = ynew-yold
         vglobal(3) = 0.0
         iret= tgc_global_to_local_p(isect,vglobal,vlocal)

         cf = abs(vlocal(1)/vlocal(2))
         theta = atan(cf)
      endif
      egr_cross_fact = 0
      end

