*
      integer function tgc_global_to_sector(isect,xglobal)
      implicit none
#include "PAM.inc"
#include "math_constants.inc"
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
      real angle
      save

C
      angle = atan2(xglobal(2),xglobal(1))
      if(angle.lt.0)angle=angle+C_2PI
      isect=int((angle+C_PI_4/3.)/(C_PI_2/3.))
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
