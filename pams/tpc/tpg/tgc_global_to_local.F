* $Id: tgc_global_to_local.F,v 1.3 1998/02/28 02:06:21 sakrejda Exp $
* $Log: tgc_global_to_local.F,v $
* Revision 1.3  1998/02/28 02:06:21  sakrejda
* math_constants.inc used in the geometry package
*
* Revision 1.2  1998/01/27 01:56:39  fisyak
* Split sources
*
      integer function tgc_global_to_local(isect,xglobal,xlocal)
      implicit none
#include "PAM.inc"
#include "math_constants.inc"
      integer isect
      real xlocal(3), xglobal(3)
C
C     Description: Conversion from global to local coordinate system
C     Input arguments:
C     xglobal - coordinates of a point in the global coordinate system
C     returns:
C     isect - sector number
C     xlocal - coordinates of a point in a local coordinate system
C
      integer iret, tgc_drift_volume_length
      integer first
      real b_rot, angle, dummy1
      real drift_vol_length
      save

      data first/1/
C
      if(first.eq.1) then
       first=0
       iret=tgc_drift_volume_length(drift_vol_length,dummy1, dummy1)
      endif
C
      angle = atan2(xglobal(2),xglobal(1))
      if(angle.lt.0)angle=angle+C_2PI
      isect=int((angle+C_PI_4/3.)/(C_PI_2/3.))
      if(isect.eq.12)isect=0
      if(xglobal(3).gt.0) then
         xlocal(3)=drift_vol_length-xglobal(3)
         isect=15-isect
         if(isect.gt.12) isect=isect-12
         b_rot=real(isect)*(C_PI_2/3.)
      else
         isect=isect+9
         if(isect.le.12) isect=isect+12
         xlocal(3)=xglobal(3)+drift_vol_length
         b_rot=real(24-isect)*(C_PI_2/3.)
      endif
      xlocal(1)=xglobal(1)*cos(b_rot)-xglobal(2)*sin(b_rot)
      xlocal(2)=xglobal(1)*sin(b_rot)+xglobal(2)*cos(b_rot)
      tgc_global_to_local=STAFCV_OK
      end
