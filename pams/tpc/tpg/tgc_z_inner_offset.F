* $Id: tgc_z_inner_offset.F,v 1.2 1998/01/27 01:56:44 fisyak Exp $
* $Log: tgc_z_inner_offset.F,v $
* Revision 1.2  1998/01/27 01:56:44  fisyak
* Split sources
*
      integer function tgc_z_inner_offset(z_offset,TPG_DETECTOR_H,TPG_DETECTOR)
      implicit none
#include "PAM.inc"
#include "tpg_detector.inc"

      RECORD / table_head_st /tpg_detector_h
      RECORD / tpg_detector_st /tpg_detector(*) 


      integer iflag/1/
      real z_offset, z_offset_local

      save

ccc
      if (iflag.eq.1) then      ! initialize: set up t0_offset table.
         iflag = 0
         z_offset_local = tpg_detector(1).z_inner_offset
      endif
ccc
      z_offset=z_offset_local
      tgc_z_inner_offset = STAFCV_OK

      end
