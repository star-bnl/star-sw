      integer function tgc_clock_frequency(clock,TPG_DETECTOR_H,TPG_DETECTOR)
      implicit none
#include "PAM.inc"
#include "tpg_detector.inc"

      RECORD / table_head_st /tpg_detector_h
      RECORD / tpg_detector_st /tpg_detector(*) 


      integer iflag/1/
      real clock, clock_local

      save

ccc
      if (iflag.eq.1) then      ! initialize: set up t0_offset table.
         iflag = 0
         clock_local = tpg_detector(1).clock_frequency
      endif
ccc
      clock=clock_local
      tgc_clock_frequency = STAFCV_OK

      end
