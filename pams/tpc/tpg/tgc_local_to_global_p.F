      integer function tgc_local_to_global_p(isect,plocal,pglobal)
      implicit none
#include "PAM.inc"
      integer isect
      real plocal(3), pglobal(3)
C     Description: Converts momentum from local to global
C     Input arguments:
C     isect - sector number
C     plocal - momentum in local coordinate system
C     Returns:
C     pglobal - momentum in global coordinate system
C     
      real  b_rot, pi_six
      pi_six = 2.0*atan(1.0)/3.0
      pglobal(3)=-plocal(3)
      if(isect.le.12) then    
        b_rot=real(isect)*pi_six
      else
        b_rot=real(24-isect)*pi_six
      endif
      pglobal(1)=plocal(1)*cos(b_rot)+plocal(2)*sin(b_rot)
      pglobal(2)=plocal(2)*cos(b_rot)-plocal(1)*sin(b_rot)
      tgc_local_to_global_p=STAFCV_OK
      end
