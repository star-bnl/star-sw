CC:>--------------------------------------------------------------------
CC: FILE:       dst_monitor_soft_filler.F
CC: HISTORY:
CC:           Created on August 12, 1998 - First Version
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION DST_MONITOR_SOFT_FILLER(
     1     tcl_tpcluster_h,     tcl_tpcluster ,
     2       scs_cluster_h,       scs_cluster ,
     3         tcl_tphit_h,         tcl_tphit ,
     4           scs_spt_h,           scs_spt ,
     5         tpt_track_h,         tpt_track ,
     6         stk_track_h,         stk_track ,
     7     svm_evt_match_h,     svm_evt_match ,
     8           ctu_cor_h,           ctu_cor ,
     9        dst_vertex_h,        dst_vertex ,
     9   dst_event_summary_h, dst_event_summary ,
     9   dst_monitor_soft_h,  dst_monitor_soft ) 
      IMPLICIT NONE
#include "dst_monitor_soft_filler.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    DST_MONITOR_SOFT_FILLER
CC: DESCRIPTION:
CC:             This module fills many of the reconstruction software
CC:             monitor variables in table dst_monitor_soft
CC: AUTHOR:     Lanny Ray, ray@physics.utexas.edu 
CC: ARGUMENTS:
CC:          IN:
CC:    tcl_tpcluster     - TPC reconstructed clusters 
CC:  tcl_tpcluster_h     - Header Structure for tcl_tpcluster
CC:      scs_cluster     - SVT reconstructed clusters
CC:    scs_cluster_h     - Header Structure for scs_cluster
CC:        tcl_tphit     - TPC reconstructed space points
CC:      tcl_tphit_h     - Header Structure for tcl_tphit
CC:          scs_spt     - SVT reconstructed space points
CC:        scs_spt_h     - Header Structure for scs_spt
CC:        tpt_track     - TPC reconstructed tracks
CC:      tpt_track_h     - Header Structure for tpt_track
CC:        stk_track     - SVT reconstructed tracks
CC:      stk_track_h     - Header Structure for stk_track
CC:    svm_evt_match     - SVT-TPC track-track match list
CC:  svm_evt_match_h     - Header Structure for svm_evt_match
CC:          ctu_cor     - CTB hits
CC:        ctu_cor_h     - Header Structure for ctu_cor
CC:       dst_vertex     - DST vertices list, includes primary vertex
CC:     dst_vertex_h     - Header Structure for dst_vertex
CC:  dst_event_summary   - DST event summary 
CC:  dst_event_summary_h - Header Structure for dst_event_summary
CC:       INOUT:
CC:         OUT:
CC:  dst_monitor_soft    - DST reconstruction software monitor info.
CC:  dst_monitor_soft_h     - Header Structure for dst_monitor_soft
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

CC:  Local Variable Type Declarations:

      integer*4 i, secnum, padrow, clusterrow,zindex,ntpc_max
      integer*4 tpc_track,n_svt_trk_pad,n_svt_trk_drf
      parameter (ntpc_max = 10000)
      integer*4 track_index(ntpc_max), tpc_trk_row

      real*4  zbin(11), zstep, zmin
      parameter (zmin = -200.0)
      parameter (zstep = 40.0)

CC:  Set dst_monitor_soft_h.nok to 1 row:

      dst_monitor_soft_h.nok = 1

CC:  Set all unused (at this time) monitor values to zero

      dst_monitor_soft(1).n_clus_ftpc(1)          = 0
      dst_monitor_soft(1).n_clus_ftpc(2)          = 0
      dst_monitor_soft(1).n_pts_ftpc(1)           = 0
      dst_monitor_soft(1).n_pts_ftpc(2)           = 0
      dst_monitor_soft(1).chrg_ftpc_tot(1)        = 0.0
      dst_monitor_soft(1).chrg_ftpc_tot(2)        = 0.0
      dst_monitor_soft(1).energy_emc              = 0.0
      dst_monitor_soft(1).n_trk_ftpc(1)           = 0
      dst_monitor_soft(1).n_trk_ftpc(2)           = 0
      dst_monitor_soft(1).hit_frac_ftpc(1)        = 0.0
      dst_monitor_soft(1).hit_frac_ftpc(2)        = 0.0
      dst_monitor_soft(1).avg_trkL_ftpc(1)        = 0.0
      dst_monitor_soft(1).avg_trkL_ftpc(2)        = 0.0
      dst_monitor_soft(1).res_pad_ftpc(1)         = 0.0
      dst_monitor_soft(1).res_pad_ftpc(2)         = 0.0
      dst_monitor_soft(1).res_drf_ftpc(1)         = 0.0
      dst_monitor_soft(1).res_drf_ftpc(2)         = 0.0

      do i = 1,20
         dst_monitor_soft(1).cpu_time(i)          = 0.0
      end do

CC:  Fill event and run ID numbers:

      dst_monitor_soft(1).n_event(1) = 
     1                    dst_event_summary(1).n_event(1)
      dst_monitor_soft(1).n_event(2) = 
     1                    dst_event_summary(1).n_event(2)
      dst_monitor_soft(1).prod_run   =
     1                    dst_event_summary(1).prod_run

CC:  Fill TPC cluster summary information:

      dst_monitor_soft(1).n_clus_tpc_tot = tcl_tpcluster_h.nok

      do i = 1,24
         dst_monitor_soft(1).n_clus_tpc_in(i)  = 0
         dst_monitor_soft(1).n_clus_tpc_out(i) = 0
      end do

      if(tcl_tpcluster_h.nok .gt. 0) then
         do i = 1,tcl_tpcluster_h.nok
            secnum = tcl_tpcluster(i).tpc_row/100
            if(secnum.ge.1 .and. secnum.le.24) then
               padrow = tcl_tpcluster(i).tpc_row - 100*secnum
               if(padrow.ge.1 .and. padrow.le.13) then
                  dst_monitor_soft(1).n_clus_tpc_in(secnum) =
     1            dst_monitor_soft(1).n_clus_tpc_in(secnum) + 1
               else if(padrow.ge.14 .and. padrow.le.45) then
                  dst_monitor_soft(1).n_clus_tpc_out(secnum) =
     1            dst_monitor_soft(1).n_clus_tpc_out(secnum) + 1
               end if
            end if
         end do
      end if

CC:  Fill SVT cluster summary information: 

      dst_monitor_soft(1).n_clus_svt = scs_cluster_h.nok 

CC:  Fill TPC reconstructed space points summary information:

      dst_monitor_soft(1).n_pts_tpc_tot = tcl_tphit_h.nok

      do i = 1,24
         dst_monitor_soft(1).n_pts_tpc_in(i)  = 0
         dst_monitor_soft(1).n_pts_tpc_out(i) = 0
      end do

      if(tcl_tphit_h.nok .gt. 0) then
         do i = 1,tcl_tphit_h.nok
               secnum = tcl_tphit(i).row/100
               if(secnum.ge.1 .and. secnum.le.24) then
                  padrow = tcl_tphit(i).row - 100*secnum
                  if(padrow.ge.1 .and. padrow.le.13) then
                     dst_monitor_soft(1).n_pts_tpc_in(secnum) =
     1               dst_monitor_soft(1).n_pts_tpc_in(secnum) + 1
                  else if(padrow.ge.14 .and. padrow.le.45) then
                     dst_monitor_soft(1).n_pts_tpc_out(secnum) =
     1               dst_monitor_soft(1).n_pts_tpc_out(secnum) + 1
                  end if
               end if
         end do
      end if

CC:  Fill SVT reconstructed space points summary information:

      dst_monitor_soft(1).n_pts_svt = scs_spt_h.nok

CC:  Fill TPC charge deposition monitor quantities:
CC:
CC:  Integrate total charge deposition in the assigned space points;
CC:  separate total charge dep. into 10 bins along z-axis (drift direction);
CC:  report total charge deposition in inner/outer sectors

      do i = 1,10
         dst_monitor_soft(1).chrg_tpc_drift(i) = 0.0
      end do
      dst_monitor_soft(1).chrg_tpc_tot = 0.0
      do i = 1,24
         dst_monitor_soft(1).chrg_tpc_in(i)  = 0.0
         dst_monitor_soft(1).chrg_tpc_out(i) = 0.0
      end do

CC:  Setup z coordinate bins:

      zbin(1) = zmin
      do i = 2,11
         zbin(i) = zbin(i-1) + zstep
      end do

CC:  Accumulate total charge deposition in TPC in 10 z-bins:

      if(tcl_tphit_h.nok .gt. 0) then
         do i = 1,tcl_tphit_h.nok
            zindex = int((tcl_tphit(i).z - zmin)/zstep + 1.000001)
            if(zindex .le. 0) zindex = 1
            if(zindex .gt. 10) zindex = 10
            dst_monitor_soft(1).chrg_tpc_drift(zindex) = 
     1      dst_monitor_soft(1).chrg_tpc_drift(zindex) + tcl_tphit(i).q
            dst_monitor_soft(1).chrg_tpc_tot = 
     1      dst_monitor_soft(1).chrg_tpc_tot + tcl_tphit(i).q

               secnum = tcl_tphit(i).row/100
               if(secnum.ge.1 .and. secnum.le.24) then
                  padrow = tcl_tphit(i).row - 100*secnum
                  if(padrow.ge.1 .and. padrow.le.13) then
                     dst_monitor_soft(1).chrg_tpc_in(secnum) = 
     1               dst_monitor_soft(1).chrg_tpc_in(secnum) + 
     2                  tcl_tphit(i).q
                  else if(padrow.ge.14 .and. padrow.le.45) then
                     dst_monitor_soft(1).chrg_tpc_out(secnum) =
     1               dst_monitor_soft(1).chrg_tpc_out(secnum) +
     2                  tcl_tphit(i).q
                  end if
               end if
         end do
      end if

CC:  Fill total charge deposition in SVT in the assigned space points

      dst_monitor_soft(1).chrg_svt_tot = 0.0

      if(scs_spt_h.nok .gt. 0) then
         do i = 1,scs_spt_h.nok
            dst_monitor_soft(1).chrg_svt_tot =
     1      dst_monitor_soft(1).chrg_svt_tot + scs_spt(i).de(1)
         end do
      end if

CC:  Sum the total number of tracks thru the CTB slats

      dst_monitor_soft(1).mult_ctb_tot = 0

      if(ctu_cor_h.nok .gt. 0) then
         do i = 1,ctu_cor_h.nok
            dst_monitor_soft(1).mult_ctb_tot = 
     1      dst_monitor_soft(1).mult_ctb_tot + ctu_cor(i).n
         end do
      end if

CC:  Sum numbers of TPC tracks, average number of assigned space points
CC:  per TPC track, and average fitting chi-square per TPC track.

      dst_monitor_soft(1).n_trk_tpc(1) = 0
      dst_monitor_soft(1).n_trk_tpc(2) = 0
      dst_monitor_soft(1).avg_trkL_tpc(1) = 0.0
      dst_monitor_soft(1).avg_trkL_tpc(2) = 0.0
      dst_monitor_soft(1).res_pad_tpc(1)  = 0.0
      dst_monitor_soft(1).res_pad_tpc(2)  = 0.0
      dst_monitor_soft(1).res_drf_tpc(1)  = 0.0
      dst_monitor_soft(1).res_drf_tpc(2)  = 0.0

      if(tpt_track_h.nok .gt. 0) then
         do i = 1,tpt_track_h.nok
            if(tpt_track(i).flag .ge. 0) then
               if(tpt_track(i).tanl .ge. 0.0) then
                  dst_monitor_soft(1).n_trk_tpc(2) = 
     1            dst_monitor_soft(1).n_trk_tpc(2) + 1
                  dst_monitor_soft(1).avg_trkL_tpc(2) = 
     1            dst_monitor_soft(1).avg_trkL_tpc(2) +
     2               float(tpt_track(i).nrec)
                  dst_monitor_soft(1).res_pad_tpc(2) =
     1               dst_monitor_soft(1).res_pad_tpc(2) +
     2               tpt_track(i).chisq(1)
                  dst_monitor_soft(1).res_drf_tpc(2) =
     1               dst_monitor_soft(1).res_drf_tpc(2) +
     2               tpt_track(i).chisq(2)
               else if(tpt_track(i).tanl .lt. 0.0) then
                  dst_monitor_soft(1).n_trk_tpc(1) = 
     1            dst_monitor_soft(1).n_trk_tpc(1) + 1
                  dst_monitor_soft(1).avg_trkL_tpc(1) = 
     1            dst_monitor_soft(1).avg_trkL_tpc(1) +
     2               float(tpt_track(i).nrec)
                  dst_monitor_soft(1).res_pad_tpc(1) =
     1               dst_monitor_soft(1).res_pad_tpc(1) +
     2               tpt_track(i).chisq(1)
                  dst_monitor_soft(1).res_drf_tpc(1) =
     1               dst_monitor_soft(1).res_drf_tpc(1) +
     2               tpt_track(i).chisq(2)

               end if
            end if
         end do
        
         if(dst_monitor_soft(1).n_trk_tpc(1) .gt. 0) then
            dst_monitor_soft(1).avg_trkL_tpc(1) =
     1         dst_monitor_soft(1).avg_trkL_tpc(1)/ 
     2         dst_monitor_soft(1).n_trk_tpc(1)
            dst_monitor_soft(1).res_pad_tpc(1) = 
     1         dst_monitor_soft(1).res_pad_tpc(1)/
     2         dst_monitor_soft(1).n_trk_tpc(1)
            dst_monitor_soft(1).res_drf_tpc(1) = 
     1         dst_monitor_soft(1).res_drf_tpc(1)/
     2         dst_monitor_soft(1).n_trk_tpc(1)
         end if
         if(dst_monitor_soft(1).n_trk_tpc(2) .gt. 0) then
            dst_monitor_soft(1).avg_trkL_tpc(2) =
     1         dst_monitor_soft(1).avg_trkL_tpc(2)/ 
     2         dst_monitor_soft(1).n_trk_tpc(2)
            dst_monitor_soft(1).res_pad_tpc(2) = 
     1         dst_monitor_soft(1).res_pad_tpc(2)/
     2         dst_monitor_soft(1).n_trk_tpc(2)
            dst_monitor_soft(1).res_drf_tpc(2) = 
     1         dst_monitor_soft(1).res_drf_tpc(2)/
     2         dst_monitor_soft(1).n_trk_tpc(2)

         end if

      end if

CC:  Count number of good SVT tracks, average number of assigned space
CC:  points per track, and average fitting chi-square per SVT track.
CC:  Include chisq outlier rejection, i.e. do not include stk_track().chisq(1)
CC:  or stk_track().chisq(2) .gt. 100 in the following averages.

      dst_monitor_soft(1).n_trk_svt = 0
      dst_monitor_soft(1).avg_trkL_svt = 0.0
      dst_monitor_soft(1).res_pad_svt  = 0.0
      dst_monitor_soft(1).res_drf_svt  = 0.0
      n_svt_trk_pad = 0
      n_svt_trk_drf = 0

      if(stk_track_h.nok .gt. 0) then
         do i = 1,stk_track_h.nok
            if(stk_track(i).flag .gt. 0) then
               dst_monitor_soft(1).n_trk_svt =
     1         dst_monitor_soft(1).n_trk_svt + 1
               dst_monitor_soft(1).avg_trkL_svt =
     1            dst_monitor_soft(1).avg_trkL_svt +
     2            stk_track(i).nspt
               if(stk_track(i).chisq(1) .le. 100.0) then
                  n_svt_trk_pad = n_svt_trk_pad + 1
                  dst_monitor_soft(1).res_pad_svt =
     1               dst_monitor_soft(1).res_pad_svt +
     2               stk_track(i).chisq(1)
               end if
               if(stk_track(i).chisq(2) .le. 100.0) then
                  n_svt_trk_drf = n_svt_trk_drf + 1
                  dst_monitor_soft(1).res_drf_svt =
     1               dst_monitor_soft(1).res_drf_svt +
     2               stk_track(i).chisq(2)
               end if
            end if
         end do

         if(dst_monitor_soft(1).n_trk_svt .gt. 0) then
            dst_monitor_soft(1).avg_trkL_svt =
     1         dst_monitor_soft(1).avg_trkL_svt/
     2         dst_monitor_soft(1).n_trk_svt
            if(n_svt_trk_pad .gt. 0) then
               dst_monitor_soft(1).res_pad_svt = 
     1            dst_monitor_soft(1).res_pad_svt/
     2            n_svt_trk_pad
            end if
            if(n_svt_trk_drf .gt. 0) then 
               dst_monitor_soft(1).res_drf_svt =
     1            dst_monitor_soft(1).res_drf_svt/
     2            n_svt_trk_drf
            end if 
         end if
      end if

CC:  Count SVT-TPC track-to-track matches and list for TPC tanl.ge.0 and <0
CC:
CC:     First, initialize arrays and build track row index array
CC:

      do i = 1,ntpc_max
         track_index(i) = 0
      end do

      dst_monitor_soft(1).n_trk_match(1) = 0
      dst_monitor_soft(1).n_trk_match(2) = 0

      if(tpt_track_h.nok .gt. 0) then
         do i = 1,tpt_track_h.nok
            if(tpt_track(i).id .gt. 0 .and. tpt_track(i).id .le.
     1         ntpc_max) then
               track_index(tpt_track(i).id) = i
            end if
         end do
      end if

CC:  Read svm_evt_match table and count # matches with tpc track's 
CC:  tanl .ge. 0 and .lt. 0

      if(svm_evt_match_h.nok .gt. 0) then
         do i = 1,svm_evt_match_h.nok
            if(svm_evt_match(i).idtpc .gt. 0 .and.
     1         svm_evt_match(i).idtpc .le. ntpc_max) then
               tpc_trk_row = track_index(svm_evt_match(i).idtpc)
               if(tpc_trk_row .gt. 0 .and. tpc_trk_row .le.
     1            tpt_track_h.nok) then
                  if(tpt_track(tpc_trk_row).tanl .ge. 0.0) then
                     dst_monitor_soft(1).n_trk_match(2) =
     1               dst_monitor_soft(1).n_trk_match(2) + 1
                  else if(tpt_track(tpc_trk_row).tanl .lt. 0.0) then
                     dst_monitor_soft(1).n_trk_match(1) =
     1               dst_monitor_soft(1).n_trk_match(1) + 1
                  end if
               end if
            end if
         end do
      end if

CC:  Fraction of TPC hits used in TPC tracks
CC:
CC:  NOTE>>>  For space points not assigned to a track, we cannot
CC:           determine if such a point "goes" with tracks that
CC:           have tanl.ge.0 or tanl.lt.0.  Therefore the used space
CC:           point fractions are computed with respect to the total
CC:           number of space points; thus causing the TPC hit fractions
CC:           to both be of order 0.5 .

      dst_monitor_soft(1).hit_frac_tpc(1) = 0.0
      dst_monitor_soft(1).hit_frac_tpc(2) = 0.0

      if(tcl_tphit_h.nok .gt. 0) then
         do i = 1,tcl_tphit_h.nok
         tpc_track = tcl_tphit(i).track/1000
            if(tpc_track .gt. 0 .and.
     1         tpc_track .le. ntpc_max) then
               tpc_trk_row = track_index(tpc_track)
               if(tpc_trk_row .gt. 0 .and. tpc_trk_row .le.
     1            tpt_track_h.nok) then
                  if(tpt_track(tpc_trk_row).tanl .ge. 0.0) then
                     dst_monitor_soft(1).hit_frac_tpc(2) =
     1               dst_monitor_soft(1).hit_frac_tpc(2) + 1.0
                  else if(tpt_track(tpc_trk_row).tanl .lt. 0.0) then
                     dst_monitor_soft(1).hit_frac_tpc(1) =
     1               dst_monitor_soft(1).hit_frac_tpc(1) + 1.0
                  end if
               end if
            end if
         end do
         dst_monitor_soft(1).hit_frac_tpc(1) =
     1   dst_monitor_soft(1).hit_frac_tpc(1)/float(tcl_tphit_h.nok)
         dst_monitor_soft(1).hit_frac_tpc(2) =
     1   dst_monitor_soft(1).hit_frac_tpc(2)/float(tcl_tphit_h.nok)
      end if

CC:  Fraction of SVT hits used in SVT tracks

      dst_monitor_soft(1).hit_frac_svt = 0.0

      if(scs_spt_h.nok .gt. 0) then
         do i = 1,scs_spt_h.nok
            if(scs_spt(i).id_track .ne. 0) then
               dst_monitor_soft(1).hit_frac_svt = 
     1         dst_monitor_soft(1).hit_frac_svt + 1.0
            end if
         end do
         dst_monitor_soft(1).hit_frac_svt =
     1      dst_monitor_soft(1).hit_frac_svt/float(scs_spt_h.nok)
      end if

CC:  Fill primary vertex monitor information

      do i = 1,10
         dst_monitor_soft(1).prim_vrtx_qual(i) = 0
      end do
      dst_monitor_soft(1).prim_vrtx_ntrk = 0

      if(dst_vertex_h.nok .gt. 0) then
         do i = 1,dst_vertex_h.nok
            if(dst_vertex(i).vtx_id .eq. 1) then
               dst_monitor_soft(1).prim_vrtx_qual(1) =
     1            dst_vertex(i).iflag
               dst_monitor_soft(1).prim_vrtx_qual(2) =
     1            int(dst_vertex(i).pchi2)
               dst_monitor_soft(1).prim_vrtx_ntrk = 
     1            dst_vertex(i).n_daughters
            end if
         end do
      end if

CC:  Successful completion of dst_monitor_soft_filler PAM


      DST_MONITOR_SOFT_FILLER=STAFCV_OK
      RETURN
      END
