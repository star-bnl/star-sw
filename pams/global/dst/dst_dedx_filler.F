CC:>--------------------------------------------------------------------
CC: FILE:       dst_dedx_filler.F
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.11 1998/06/23 15:52:43 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION DST_DEDX_FILLER(
     1         tpt_track_h,         tpt_track ,
     2         stk_track_h,         stk_track ,
     3          tpc_dedx_h,         tpc_dedx,
     4          dst_dedx_h,          dst_dedx ) 
      IMPLICIT NONE
#include "dst_dedx_filler.inc"
#include "StDetectorId.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    DST_DEDX_FILLER
CC: DESCRIPTION:
CC:             This module reads the tpc track table, tpt_track, and
CC:             tpc_dedx table and
CC:             determines which tracks have been assigned to a global
CC:             track which are listed in the dst_track table.
CC:             For all such tracks the module copies the dE/dx information
CC:             from the tpc_dedx table (which only knew about tpc tracks
CC:             at its creation into the dst_dedx table. the dst_dedx
CC:             tables is indexed by global trackid.  Note that the 
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
CC:         tpc_dedx     - tpc dedx table
cc:         tpc_dedx_h   - header structure for dst-dedx
CC:       INOUT:
CC:         OUT:
CC:         dst_dedx     - DST dE/dx summary table 
CC:       dst_dedx_h     - Header Structure for dst_dedx
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

CC:  Local Variable Type Declarations:

      integer*4 num_tpc_trks,num_svt_trks,num_tpc_dedx,i,j,k
      logical found
CC:  Determine number of tpc and svt tracks

      num_tpc_trks = tpt_track_h.nok
      num_svt_trks = stk_track_h.nok
      num_tpc_dedx = tpc_dedx_h.nok
c      write(6,*) "in filler", num_tpc_trks,num_tpc_dedx

CC:  Loop thru all rows in tpc_dedx:

      if(num_tpc_dedx .ge. 1 .and. dst_dedx_h.maxlen .ge. 1) then
       do i = 1,num_tpc_dedx
         k=1
         found = .false.
         do while (k.le.num_tpc_trks .and. found.eq..false.)
            if (tpc_dedx(i).id_track .eq. tpt_track(k).id) then
c
c found the tpc track
c
               found = .true.
            else
               k=k+1
            endif
         enddo
         if (found) then
             if(tpt_track(k).id_globtrk .gt. 0
     1       .and. dst_dedx_h.nok .lt. dst_dedx_h.maxlen) then
                 dst_dedx_h.nok = dst_dedx_h.nok + 1
                 j = dst_dedx_h.nok
                 dst_dedx(j).id_track  =  tpt_track(k).id_globtrk
                 dst_dedx(j).det_id    =  kTpcId    ! TPC track 
                 dst_dedx(j).method    =  tpc_dedx(i).method
                 dst_dedx(j).ndedx     =  tpc_dedx(i).ndedx
                 dst_dedx(j).dedx(1)   =  tpc_dedx(i).dedx(1)
                 dst_dedx(j).dedx(2)   =  tpc_dedx(i).dedx(2)
            else if(dst_dedx_h.nok .ge. dst_dedx_h.maxlen) then
                 write(6,10) dst_dedx_h.nok
10               Format(2x,'No. DST DEDX entries .ge. maximum')
                 DST_DEDX_FILLER = STAFCV_BAD
                 Return
            end if
         else
            write (6,*) "no tpc track found. Looked for id: ",tpc_dedx(i).id_track
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
            dst_dedx(j).det_id    =  kSvtId    ! SVT track
            dst_dedx(j).method     =  0
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
