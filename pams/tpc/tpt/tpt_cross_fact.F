      INTEGER FUNCTION TPT_CROSS_FACT(idet,xnew,ynew,xold,yold,cf,theta)
C>------------------------------------------------------------------------
C
C TPT_CROSS_FACT - evaluates crossing angle and weighting factor 
C                  for error
C
C DESCRIPTION:
C The angle is evaluated from a dot prodact of a vector 
C tangent to the track and a vector parallel to the row.
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
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE
#include "tpt_const.inc"
C______________________________________________________________________
      INTEGER idet
      REAL    xnew,ynew,xold,yold,cf,theta
      real    vlocal(3),vglobal(3)
      REAL    pi,alpha
      integer iret,tgc_global_to_local_p,isect
      PARAMETER (pi=3.141592653589)
C_____________________________________________________________________

      if(cf.lt.0) then 
         cf=1.0
      else
         isect = idet/100
         alpha = real(2*isect-1)*pi/12.0
         vglobal(1)= xnew-xold
         vglobal(2)= ynew-yold
         vglobal(3)= 0.0
         iret= tgc_global_to_local_p(isect,vglobal,vlocal)
C
         cf = abs(vlocal(1)/vlocal(2))
         theta = atan(cf)
      endif
      tpt_cross_fact = 0
      end
