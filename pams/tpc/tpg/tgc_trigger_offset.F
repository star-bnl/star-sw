
*
      integer function tgc_trigger_offset(trigger_offset,TPG_DETECTOR_H,TPG_DETECTOR)
      implicit none
#include "PAM.inc"
#include "tpg_detector.inc"

      RECORD / table_head_st /tpg_detector_h
      RECORD / tpg_detector_st /tpg_detector(*) 


      integer iflag/1/
      real trigger_offset, trigger_offset_local

      save

ccc
      if (iflag.eq.1) then      ! initialize: set up t0_offset table.
         iflag = 0
         trigger_offset_local = tpg_detector(1).trigger_offset
      endif
ccc
      trigger_offset=trigger_offset_local
      tgc_trigger_offset = STAFCV_OK

      end
