* $Id: tgc_drift_velocity.F,v 1.2 1998/01/27 01:56:39 fisyak Exp $
* $Log: tgc_drift_velocity.F,v $
* Revision 1.2  1998/01/27 01:56:39  fisyak
* Split sources
*
      integer function tgc_drift_velocity(dvel,TPG_DETECTOR_H,TPG_DETECTOR)
      implicit none
#include "PAM.inc"
#include "tpg_detector.inc"

      RECORD / table_head_st /tpg_detector_h
      RECORD / tpg_detector_st /tpg_detector(*) 


      integer iflag/1/
      real dvel, dvel_local

      save

ccc
      if (iflag.eq.1) then      ! initialize: set up t0_offset table.
         iflag = 0
         dvel_local = tpg_detector(1).vdrift
      endif
ccc
      dvel = dvel_local
      tgc_drift_velocity = STAFCV_OK

      end
