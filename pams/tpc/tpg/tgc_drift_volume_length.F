      integer function tgc_drift_volume_length(xlength,
     >                   tpg_detector_h, tpg_detector)
      implicit none
c     tpg_detector_h
c     tpg_detector - contains drift distance length

#include "PAM.inc"
#include "tpg_detector.inc"

      RECORD / table_head_st /tpg_detector_h
      RECORD / tpg_detector_st /tpg_detector(*)

      real    length , xlength
      integer flag

      save

      data flag /1/
C
      if(flag.eq.1) then
          length=tpg_detector(1).drift_length
          flag=0
      endif
      xlength=length
      tgc_drift_volume_length=STAFCV_OK
      end
