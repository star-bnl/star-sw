* $Id: tgc_local_to_global.F,v 1.7 1998/11/19 21:06:36 sakrejda Exp $
* $Log: tgc_local_to_global.F,v $
* Revision 1.7  1998/11/19 21:06:36  sakrejda
* transformations updated one more time, this time checked by Lidia for simulations and Eric for test data
*
* Revision 1.5  1998/11/18 04:41:07  sakrejda
* transformations for sector<13 were broken, fixed them
*
* Revision 1.4  1998/11/10 01:25:00  sakrejda
* conversion between loccal and global fixed for z>0, there rotation is actually counterclockwise
*
* Revision 1.3  1998/02/28 02:06:22  sakrejda
* math_constants.inc used in the geometry package
*
* Revision 1.2  1998/01/27 01:56:41  fisyak
* Split sources
*
      integer function tgc_local_to_global(isect,xlocal,xglobal)
      implicit none
#include "PAM.inc"
#include "math_constants.inc"

      integer isect
      real xlocal(3), xglobal(3)

C     Description: Conversion from local to global coordinate system
C     Input arguments:
C     isect  - sector number
C     xlocal - coordinates of a point in the local coordinate system
C     Returns:
C     xglobal- coordinates of a point in the global coordinate system
C
      integer iret, tgc_drift_volume_length
      real b_rot, drift_vol_length, dummy1, xl
*
      iret=tgc_drift_volume_length(drift_vol_length, dummy1, dummy1)
      if(isect.le.12) then    
        b_rot=real(isect)*(C_PI_2/3.)
        xglobal(3)=drift_vol_length-xlocal(3)
        xl=-xlocal(1)
      else
        b_rot=real(24-isect)*(C_PI_2/3.)
        xglobal(3)=-drift_vol_length+xlocal(3)
        xl=xlocal(1)
      endif
      xglobal(1)=xl*cos(b_rot)+xlocal(2)*sin(b_rot)
      xglobal(2)=xlocal(2)*cos(b_rot)-xl*sin(b_rot)
      tgc_local_to_global=STAFCV_OK
      end
