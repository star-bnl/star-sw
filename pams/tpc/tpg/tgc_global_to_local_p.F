* $Id: tgc_global_to_local_p.F,v 1.8 1998/12/08 05:47:27 sakrejda Exp $
* $Log: tgc_global_to_local_p.F,v $
* Revision 1.8  1998/12/08 05:47:27  sakrejda
* transformation global->local fixed, it was messed up to compensate for a tss problem
*
* Revision 1.7  1998/11/19 21:06:35  sakrejda
* transformations updated one more time, this time checked by Lidia for simulations and Eric for test data
*
* Revision 1.5  1998/11/18 04:41:07  sakrejda
* transformations for sector<13 were broken, fixed them
*
* Revision 1.4  1998/11/10 01:24:59  sakrejda
* conversion between loccal and global fixed for z>0, there rotation is actually counterclockwise
*
* Revision 1.3  1998/02/28 02:06:21  sakrejda
* math_constants.inc used in the geometry package
*
* Revision 1.2  1998/01/27 01:56:40  fisyak
* Split sources
*
      integer function tgc_global_to_local_p(isect,pglobal,plocal)
      implicit none
#include "PAM.inc"
#include "math_constants.inc"

      integer isect
      real plocal(3)
      real pglobal(3)
C     Description: Converts momentum from global to local coordinate
C     system
C     Input arguments:
C     Returns:
C     plocal - momentum in local coordinate system

      real b_rot, ff
      plocal(3)=-pglobal(3)
      if(isect.le.12) then    
        b_rot=real(isect)*(C_PI_2/3.)
        ff=-1.0
      else
        b_rot=real(24-isect)*(C_PI_2/3.)
        ff=1.0
      endif
      plocal(1)=ff*(pglobal(1)*cos(b_rot)-pglobal(2)*sin(b_rot))
      plocal(2)=pglobal(1)*sin(b_rot)+pglobal(2)*cos(b_rot)
      tgc_global_to_local_p=STAFCV_OK
      end
