      integer function tgc_global_to_local_p(isect,pglobal,plocal)
      implicit none
#include "PAM.inc"

      integer isect
      real plocal(3)
      real pglobal(3)
C     Description: Converts momentum from global to local coordinate
C     system
C     Input arguments:
C     Returns:
C     plocal - momentum in local coordinate system

      real b_rot, pi_six
      plocal(3)=-pglobal(3)
      pi_six = 2.0*atan(1.0)/3.0
      if(isect.le.12) then    
        b_rot=real(isect)*pi_six
      else
        b_rot=real(24-isect)*pi_six
      endif
      plocal(1)=pglobal(1)*cos(b_rot)-pglobal(2)*sin(b_rot)
      plocal(2)=pglobal(1)*sin(b_rot)+pglobal(2)*cos(b_rot)
      tgc_global_to_local_p=STAFCV_OK
      end
