CC:>--------------------------------------------------------------------
CC: FILE:       dst_dedx_filler.F
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.11 1998/06/23 15:52:43 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION DST_DEDX_FILLER(
     1         tpt_track_h,         tpt_track ,
     2         stk_track_h,         stk_track ,
     3          dst_dedx_h,          dst_dedx ) 
      IMPLICIT NONE
#include "dst_dedx_filler.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    DST_DEDX_FILLER
CC: DESCRIPTION:
CC:             This module reads the tpc track table, tpt_track, and
CC:             determines which tracks have been assigned to a global
CC:             track which are listed in the dst_track table.
CC:             For all such tracks the module copies the dE/dx information
CC:             in tpt_track into the dst_dedx table.  Note that the 
CC:             global track refitter pkg, egr, must be run prior to 
CC:             running this module.
CC:             The svt track table stk_track was added (8/18/98) and
CC:             the svt dE/dx values input to the dst_dedx table.
CC: AUTHOR:     R. L. Ray, ray@physics.utexas.edu
CC: ARGUMENTS:
CC:          IN:
CC:        tpt_track     - TPC track table
CC:      tpt_track_h     - Header Structure for tpt_track
CC:        stk_track     - SVT track table
CC:      stk_track_h     - Header Structure for stk_track
CC:       INOUT:
CC:         OUT:
CC:         dst_dedx     - DST dE/dx summary table 
CC:       dst_dedx_h     - Header Structure for dst_dedx
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

CC:  Local Variable Type Declarations:

      integer*4 num_tpc_trks,num_svt_trks,i,j

CC:  Determine number of tpc and svt tracks

      num_tpc_trks = tpt_track_h.nok
      num_svt_trks = stk_track_h.nok

CC:  Loop thru all rows in tpt_track:

      if(num_tpc_trks .ge. 1 .and. dst_dedx_h.maxlen .ge. 1) then
       do i = 1,num_tpc_trks
         if(tpt_track(i).id_globtrk .gt. 0
     1      .and. dst_dedx_h.nok .lt. dst_dedx_h.maxlen) then
            dst_dedx_h.nok = dst_dedx_h.nok + 1
            j = dst_dedx_h.nok
            dst_dedx(j).id_track  =  tpt_track(i).id_globtrk
            dst_dedx(j).det_id    =  1    ! TPC track 
            dst_dedx(j).iflag     =  0
            dst_dedx(j).ndedx     =  tpt_track(i).ndedx
            dst_dedx(j).dedx(1)   =  tpt_track(i).dedx(1)
            dst_dedx(j).dedx(2)   =  tpt_track(i).dedx(2)
         else if(dst_dedx_h.nok .ge. dst_dedx_h.maxlen) then
            write(6,10) dst_dedx_h.nok
10          Format(2x,'No. DST DEDX entries .ge. maximum')
            DST_DEDX_FILLER = STAFCV_BAD
            Return
         end if
       end do
      end if

CC:  Loop thru all rows in stk_track:

      if(num_svt_trks .ge. 1 .and. dst_dedx_h.maxlen .ge. 1) then
       do i = 1,num_svt_trks
         if(stk_track(i).id_globtrk .gt. 0
     1      .and. dst_dedx_h.nok .lt. dst_dedx_h.maxlen) then
            dst_dedx_h.nok = dst_dedx_h.nok + 1
            j = dst_dedx_h.nok
            dst_dedx(j).id_track  =  stk_track(i).id_globtrk
            dst_dedx(j).det_id    =  2    ! SVT track
            dst_dedx(j).iflag     =  0
            dst_dedx(j).ndedx     =  0
            dst_dedx(j).dedx(1)   =  stk_track(i).dedx(1)
            dst_dedx(j).dedx(2)   =  stk_track(i).dedx(2)
         else if(dst_dedx_h.nok .ge. dst_dedx_h.maxlen) then
            write(6,10) dst_dedx_h.nok
            DST_DEDX_FILLER = STAFCV_BAD
            Return
         end if
       end do
      end if

C: Successful completion of analysis module...
      DST_DEDX_FILLER = STAFCV_OK
      RETURN
      END
