      integer function tgc_sec24_to_sec12(isecin,isecout)

      implicit none

#include "PAM.inc"
      integer isecin, isecout
      integer isec24(24)
      data isec24 /1,2,3,4,5,6,7,8,9,10,11,12,11,10,9,8,7,6,5,
     >             4,3,2,1,12/
      save
      if(isecin.ge.1.and.isecin.lt.25) isecout=isec24(isecin)

      tgc_sec24_to_sec12=STAFCV_OK
      end

