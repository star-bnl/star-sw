*
      integer function tgc_global_to_sector(isect,xglobal)
      implicit none
#include "PAM.inc"
      integer isect
      real xglobal(3)
C
C     Description: Conversion from global to sector number
C     Input arguments:
C     xglobal - coordinates of a point in the global coordinate system
C     returns:
C     isect - sector number
C
      integer iret
      integer first
      real angle
      real twopi, pi_twelve, pi_six
      save

      data first/1/
C
      if(first.eq.1) then
       twopi=8.0*atan(1.0)
       pi_twelve=atan(1.0)/3.0
       pi_six=pi_twelve*2.0
       first=0
      endif
C
      angle = atan2(xglobal(2),xglobal(1))
      if(angle.lt.0)angle=angle+twopi
      isect=int((angle+pi_twelve)/pi_six)
      if(isect.eq.12)isect=0
      if(xglobal(3).gt.0) then
         isect=15-isect
         if(isect.gt.12) isect=isect-12
      else
         isect=isect+9
         if(isect.le.12) isect=isect+12
      endif

      tgc_global_to_sector=STAFCV_OK
      end
