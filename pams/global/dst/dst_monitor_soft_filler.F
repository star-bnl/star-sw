CC:>--------------------------------------------------------------------
CC: FILE:       dst_monitor_soft_filler.F
CC: HISTORY:
CC:           Created on August 12, 1998 - First Version
CC:
CC:           Modified on January 21, 1999 - include FTPC data
CC:                       Janet Seyboth, jcs@mppmu.mpg.de
CC:            
CC:           Modified September 1999 - split output into several tables
CC:                       Lee Barnby,  lbarnby@bnl.gov
CC:
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION DST_MONITOR_SOFT_FILLER(
     1     tcl_tpcluster_h,     tcl_tpcluster ,
     2       scs_cluster_h,       scs_cluster ,
     3         tcl_tphit_h,         tcl_tphit ,
     4           scs_spt_h,           scs_spt ,
     4       fcl_fppoint_h,       fcl_fppoint ,
     5         tpt_track_h,         tpt_track ,
     6         stk_track_h,         stk_track ,
     6       fpt_fptrack_h,       fpt_fptrack ,
     7     svm_evt_match_h,     svm_evt_match ,
     8           ctu_cor_h,           ctu_cor ,
     9        dst_vertex_h,        dst_vertex ,
     9   dst_event_summary_h, dst_event_summary ,
     9   dms_ftpc_h,          dms_ftpc , 
     9   dms_global_h,        dms_global , 
     9   dms_svt_h,           dms_svt , 
     9   dms_tpc_h,           dms_tpc ) 

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
CC:      fcl_fppoint     - FTPC reconstructed space points
CC:    fcl_fppoint_h     - Header Structure for fcl_fppoint
CC:        tpt_track     - TPC reconstructed tracks
CC:      tpt_track_h     - Header Structure for tpt_track
CC:        stk_track     - SVT reconstructed tracks
CC:      stk_track_h     - Header Structure for stk_track
CC:      fpt_fptrack     - FTPC reconstructed tracks
CC:    fpt_fptrack_h     - Header Structure for fpt_fptrack
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
CC:  dms_ftpc    - DST reconstruction software monitor info.
CC:  dms_ftpc_h   - Header Structure for dms_ftpc
CC:  dms_global  - DST reconstruction software monitor info.
CC:  dms_global_h - Header Structure for dms_global
CC:  dms_svt     - DST reconstruction software monitor info.
CC:  dms_svt_h    - Header Structure for dms_svt
CC:  dms_tpc     - DST reconstruction software monitor info.
CC:  dms_tpc_h    - Header Structure for dms_tpc
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

CC:  Local Variable Type Declarations:

      integer*4 i, j, secnum, padrow, clusterrow,zindex,ntpc_max
      integer*4 tpc_track,n_svt_trk_pad,n_svt_trk_drf,layer
      parameter (ntpc_max = 20000)
      integer*4 track_index(ntpc_max), tpc_trk_row
      integer*4 iftpc, ihit, nhits(2)
      integer*4 max_ftpc_hits
      parameter (max_ftpc_hits = 10)

      real*4  zbin(11), zstep, zmin
      parameter (zmin = -200.0)
      parameter (zstep = 40.0)

CC:  Set (all) dms_h***.nok to 1 row:

CC: emc and ctb not ready to go in yet (LSB)
CC:      dms_ctb-tof_h.nok = 1
CC:      dms_emc_h.nok = 1
      dms_ftpc_h.nok = 1
      dms_global_h.nok = 1
      dms_svt_h.nok = 1
      dms_tpc_h.nok = 1

CC:  Set all unused (at this time) monitor values to zero

      dms_ftpc(1).n_clus_ftpc(1)          = 0
      dms_ftpc(1).n_clus_ftpc(2)          = 0
CC: Monitor table for emc not ready yet (LSB)
CC:      dms_emc(1).energy_emc              = 0.0

CC: CPU time no longer appears? (LSB)
CC:      do i = 1,20
CC:         dst_monitor_soft(1).cpu_time(i)          = 0.0
CC:      end do

CC:  Fill event and run ID numbers:

CC:  event and prod_run not in monitor_soft tables now? (LSB)
CC:      dst_monitor_soft(1).n_event(1) = 
CC:     1                    dst_event_summary(1).n_event(1)
CC:      dst_monitor_soft(1).n_event(2) = 
CC:     1                    dst_event_summary(1).n_event(2)
CC:      dst_monitor_soft(1).prod_run   =
CC:     1                    dst_event_summary(1).prod_run

CC:  Fill TPC cluster summary information:

      dms_tpc(1).n_clus_tpc_tot = tcl_tpcluster_h.nok

      do i = 1,24
         dms_tpc(1).n_clus_tpc_in(i)  = 0
         dms_tpc(1).n_clus_tpc_out(i) = 0
      end do

      if(tcl_tpcluster_h.nok .gt. 0) then
         do i = 1,tcl_tpcluster_h.nok
            secnum = tcl_tpcluster(i).tpc_row/100
            if(secnum.ge.1 .and. secnum.le.24) then
               padrow = tcl_tpcluster(i).tpc_row - 100*secnum
               if(padrow.ge.1 .and. padrow.le.13) then
                  dms_tpc(1).n_clus_tpc_in(secnum) =
     1            dms_tpc(1).n_clus_tpc_in(secnum) + 1
               else if(padrow.ge.14 .and. padrow.le.45) then
                  dms_tpc(1).n_clus_tpc_out(secnum) =
     1            dms_tpc(1).n_clus_tpc_out(secnum) + 1
               end if
            end if
         end do
      end if

CC:  Fill SVT cluster summary information: 
CC:  cluster not yet available due to lack of slow sim/real data (LSB)

      dms_svt(1).n_clus_svt(1) = 0
      dms_svt(1).n_clus_svt(2) = 0 
      dms_svt(1).n_clus_svt(3) = 0 
      dms_svt(1).n_clus_svt(4) = 0

      if(scs_cluster_h.nok .gt. 0) then
CC:     code to unpack cluster and count layers goes in here when ready
      end if

CC:  Fill TPC reconstructed space points summary information:

      dms_tpc(1).n_pts_tpc_tot = tcl_tphit_h.nok

      do i = 1,24
         dms_tpc(1).n_pts_tpc_in(i)  = 0
         dms_tpc(1).n_pts_tpc_out(i) = 0
      end do

      if(tcl_tphit_h.nok .gt. 0) then
         do i = 1,tcl_tphit_h.nok
               secnum = tcl_tphit(i).row/100
               if(secnum.ge.1 .and. secnum.le.24) then
                  padrow = tcl_tphit(i).row - 100*secnum
                  if(padrow.ge.1 .and. padrow.le.13) then
                     dms_tpc(1).n_pts_tpc_in(secnum) =
     1               dms_tpc(1).n_pts_tpc_in(secnum) + 1
                  else if(padrow.ge.14 .and. padrow.le.45) then
                     dms_tpc(1).n_pts_tpc_out(secnum) =
     1               dms_tpc(1).n_pts_tpc_out(secnum) + 1
                  end if
               end if
         end do
      end if

CC:  Fill SVT reconstructed space points summary information:

      dms_svt(1).n_pts_svt(1) = 0
      dms_svt(1).n_pts_svt(2) = 0
      dms_svt(1).n_pts_svt(3) = 0
      dms_svt(1).n_pts_svt(4) = 0

      if(scs_spt_h.nok .gt. 0) then
         do i = 1,scs_spt_h.nok
            layer=((scs_spt(i).id_wafer/1000)+1)/2
            if(layer .le. 4 .and. layer .gt. 0) then
               dms_svt(1).n_pts_svt(layer) =
     1         dms_svt(1).n_pts_svt(layer) + 1
            end if
         end do
      end if

CC:  Fill TPC charge deposition monitor quantities:
CC:
CC:  Integrate total charge deposition in the assigned space points;
CC:  separate total charge dep. into 10 bins along z-axis (drift direction);
CC:  report total charge deposition in inner/outer sectors

      do i = 1,10
         dms_tpc(1).chrg_tpc_drift(i) = 0.0
      end do
      dms_tpc(1).chrg_tpc_tot = 0.0
      do i = 1,24
         dms_tpc(1).chrg_tpc_in(i)  = 0.0
         dms_tpc(1).chrg_tpc_out(i) = 0.0
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
            dms_tpc(1).chrg_tpc_drift(zindex) = 
     1      dms_tpc(1).chrg_tpc_drift(zindex) + tcl_tphit(i).q
            dms_tpc(1).chrg_tpc_tot = 
     1      dms_tpc(1).chrg_tpc_tot + tcl_tphit(i).q

               secnum = tcl_tphit(i).row/100
               if(secnum.ge.1 .and. secnum.le.24) then
                  padrow = tcl_tphit(i).row - 100*secnum
                  if(padrow.ge.1 .and. padrow.le.13) then
                     dms_tpc(1).chrg_tpc_in(secnum) = 
     1               dms_tpc(1).chrg_tpc_in(secnum) + 
     2                  tcl_tphit(i).q
                  else if(padrow.ge.14 .and. padrow.le.45) then
                     dms_tpc(1).chrg_tpc_out(secnum) =
     1               dms_tpc(1).chrg_tpc_out(secnum) +
     2                  tcl_tphit(i).q
                  end if
               end if
         end do
      end if

CC:  Fill total charge deposition in SVT in the assigned space points     

      dms_svt(1).chrg_svt_tot(1) = 0.0
      dms_svt(1).chrg_svt_tot(2) = 0.0
      dms_svt(1).chrg_svt_tot(3) = 0.0
      dms_svt(1).chrg_svt_tot(4) = 0.0

      if(scs_spt_h.nok .gt. 0) then
         do i = 1,scs_spt_h.nok
            layer=((scs_spt(i).id_wafer/1000)+1)/2
            if(layer .le. 4 .and. layer .gt. 0) then
               dms_svt(1).chrg_svt_tot(layer) =
     1         dms_svt(1).chrg_svt_tot(layer) + scs_spt(i).de(1)
         end if
         end do
      end if

CC:  Sum the total number of tracks thru the CTB slats

CC:  ctb-tof table not ready for DST yet (LSB)
CC:      dms_ctb-tof(1).mult_ctb_tot = 0

CC:      if(ctu_cor_h.nok .gt. 0) then
CC:         do i = 1,ctu_cor_h.nok
CC:            dms_ctb-tof(1).mult_ctb_tot = 
CC:     1      dms_ctb-tof(1).mult_ctb_tot + ctu_cor(i).n
CC:         end do
CC:      end if

CC:  Sum numbers of TPC tracks, average number of assigned space points
CC:  per TPC track, and average fitting chi-square per TPC track.

      dms_tpc(1).n_trk_tpc(1) = 0
      dms_tpc(1).n_trk_tpc(2) = 0
      dms_tpc(1).avg_trkL_tpc(1) = 0.0
      dms_tpc(1).avg_trkL_tpc(2) = 0.0
      dms_tpc(1).res_pad_tpc(1)  = 0.0
      dms_tpc(1).res_pad_tpc(2)  = 0.0
      dms_tpc(1).res_drf_tpc(1)  = 0.0
      dms_tpc(1).res_drf_tpc(2)  = 0.0

      if(tpt_track_h.nok .gt. 0) then
         do i = 1,tpt_track_h.nok
            if(tpt_track(i).flag .ge. 0) then
               if(tpt_track(i).tanl .ge. 0.0) then
                  dms_tpc(1).n_trk_tpc(2) = 
     1            dms_tpc(1).n_trk_tpc(2) + 1
                  dms_tpc(1).avg_trkL_tpc(2) = 
     1            dms_tpc(1).avg_trkL_tpc(2) +
     2               float(tpt_track(i).nrec)
                  dms_tpc(1).res_pad_tpc(2) =
     1               dms_tpc(1).res_pad_tpc(2) +
     2               tpt_track(i).chisq(1)
                  dms_tpc(1).res_drf_tpc(2) =
     1               dms_tpc(1).res_drf_tpc(2) +
     2               tpt_track(i).chisq(2)
               else if(tpt_track(i).tanl .lt. 0.0) then
                  dms_tpc(1).n_trk_tpc(1) = 
     1            dms_tpc(1).n_trk_tpc(1) + 1
                  dms_tpc(1).avg_trkL_tpc(1) = 
     1            dms_tpc(1).avg_trkL_tpc(1) +
     2               float(tpt_track(i).nrec)
                  dms_tpc(1).res_pad_tpc(1) =
     1               dms_tpc(1).res_pad_tpc(1) +
     2               tpt_track(i).chisq(1)
                  dms_tpc(1).res_drf_tpc(1) =
     1               dms_tpc(1).res_drf_tpc(1) +
     2               tpt_track(i).chisq(2)

               end if
            end if
         end do
        
         if(dms_tpc(1).n_trk_tpc(1) .gt. 0) then
            dms_tpc(1).avg_trkL_tpc(1) =
     1         dms_tpc(1).avg_trkL_tpc(1)/ 
     2         dms_tpc(1).n_trk_tpc(1)
            dms_tpc(1).res_pad_tpc(1) = 
     1         dms_tpc(1).res_pad_tpc(1)/
     2         dms_tpc(1).n_trk_tpc(1)
            dms_tpc(1).res_drf_tpc(1) = 
     1         dms_tpc(1).res_drf_tpc(1)/
     2         dms_tpc(1).n_trk_tpc(1)
         end if
         if(dms_tpc(1).n_trk_tpc(2) .gt. 0) then
            dms_tpc(1).avg_trkL_tpc(2) =
     1         dms_tpc(1).avg_trkL_tpc(2)/ 
     2         dms_tpc(1).n_trk_tpc(2)
            dms_tpc(1).res_pad_tpc(2) = 
     1         dms_tpc(1).res_pad_tpc(2)/
     2         dms_tpc(1).n_trk_tpc(2)
            dms_tpc(1).res_drf_tpc(2) = 
     1         dms_tpc(1).res_drf_tpc(2)/
     2         dms_tpc(1).n_trk_tpc(2)

         end if

      end if

CC:  Count number of good SVT tracks, average number of assigned space
CC:  points per track, and average fitting chi-square per SVT track.
CC:  Include chisq outlier rejection, i.e. do not include stk_track().chisq(1)
CC:  or stk_track().chisq(2) .gt. 100 in the following averages.

      dms_svt(1).n_trk_svt = 0
      dms_svt(1).avg_trkL_svt = 0.0
      dms_svt(1).res_pad_svt  = 0.0
      dms_svt(1).res_drf_svt  = 0.0
      n_svt_trk_pad = 0
      n_svt_trk_drf = 0

      if(stk_track_h.nok .gt. 0) then
         do i = 1,stk_track_h.nok
            if(stk_track(i).flag .gt. 0) then
               dms_svt(1).n_trk_svt =
     1         dms_svt(1).n_trk_svt + 1
               dms_svt(1).avg_trkL_svt =
     1            dms_svt(1).avg_trkL_svt +
     2            stk_track(i).nspt
               if(stk_track(i).chisq(1) .le. 100.0) then
                  n_svt_trk_pad = n_svt_trk_pad + 1
                  dms_svt(1).res_pad_svt =
     1               dms_svt(1).res_pad_svt +
     2               stk_track(i).chisq(1)
               end if
               if(stk_track(i).chisq(2) .le. 100.0) then
                  n_svt_trk_drf = n_svt_trk_drf + 1
                  dms_svt(1).res_drf_svt =
     1               dms_svt(1).res_drf_svt +
     2               stk_track(i).chisq(2)
               end if
            end if
         end do

         if(dms_svt(1).n_trk_svt .gt. 0) then
            dms_svt(1).avg_trkL_svt =
     1         dms_svt(1).avg_trkL_svt/
     2         dms_svt(1).n_trk_svt
            if(n_svt_trk_pad .gt. 0) then
               dms_svt(1).res_pad_svt = 
     1            dms_svt(1).res_pad_svt/
     2            n_svt_trk_pad
            end if
            if(n_svt_trk_drf .gt. 0) then 
               dms_svt(1).res_drf_svt =
     1            dms_svt(1).res_drf_svt/
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

      dms_global(1).n_trk_match(1) = 0
      dms_global(1).n_trk_match(2) = 0

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
                     dms_global(1).n_trk_match(2) =
     1               dms_global(1).n_trk_match(2) + 1
                  else if(tpt_track(tpc_trk_row).tanl .lt. 0.0) then
                     dms_global(1).n_trk_match(1) =
     1               dms_global(1).n_trk_match(1) + 1
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

      dms_tpc(1).hit_frac_tpc(1) = 0.0
      dms_tpc(1).hit_frac_tpc(2) = 0.0

      if(tcl_tphit_h.nok .gt. 0) then
         do i = 1,tcl_tphit_h.nok
         tpc_track = tcl_tphit(i).track/1000
            if(tpc_track .gt. 0 .and.
     1         tpc_track .le. ntpc_max) then
               tpc_trk_row = track_index(tpc_track)
               if(tpc_trk_row .gt. 0 .and. tpc_trk_row .le.
     1            tpt_track_h.nok) then
                  if(tpt_track(tpc_trk_row).tanl .ge. 0.0) then
                     dms_tpc(1).hit_frac_tpc(2) =
     1               dms_tpc(1).hit_frac_tpc(2) + 1.0
                  else if(tpt_track(tpc_trk_row).tanl .lt. 0.0) then
                     dms_tpc(1).hit_frac_tpc(1) =
     1               dms_tpc(1).hit_frac_tpc(1) + 1.0
                  end if
               end if
            end if
         end do
         dms_tpc(1).hit_frac_tpc(1) =
     1   dms_tpc(1).hit_frac_tpc(1)/float(tcl_tphit_h.nok)
         dms_tpc(1).hit_frac_tpc(2) =
     1   dms_tpc(1).hit_frac_tpc(2)/float(tcl_tphit_h.nok)
      end if

CC:  Fraction of SVT hits used in SVT tracks

      dms_svt(1).hit_frac_svt(1) = 0.0
      dms_svt(1).hit_frac_svt(2) = 0.0
      dms_svt(1).hit_frac_svt(3) = 0.0
      dms_svt(1).hit_frac_svt(4) = 0.0

      if(scs_spt_h.nok .gt. 0) then
         do i = 1,scs_spt_h.nok
            layer=((scs_spt(i).id_wafer/1000)+1)/2
            if(layer .le. 4 .and. layer .gt. 0) then
               if(scs_spt(i).id_track .ne. 0) then
                  dms_svt(1).hit_frac_svt(layer) = 
     1            dms_svt(1).hit_frac_svt(layer) + 1.0
               end if
            end if
         end do
         do i = 1,4
         dms_svt(1).hit_frac_svt(layer) =
     1      dms_svt(1).hit_frac_svt(layer)/
     2           float(dms_svt(1).n_pts_svt(layer))
         end do
      end if

CC:  Fill FTPC monitor information

      dms_ftpc(1).n_pts_ftpc(1)           = 0
      dms_ftpc(1).n_pts_ftpc(2)           = 0
      dms_ftpc(1).chrg_ftpc_tot(1)        = 0.0
      dms_ftpc(1).chrg_ftpc_tot(2)        = 0.0
      dms_ftpc(1).n_trk_ftpc(1)           = 0
      dms_ftpc(1).n_trk_ftpc(2)           = 0
      dms_ftpc(1).hit_frac_ftpc(1)        = 0.0
      dms_ftpc(1).hit_frac_ftpc(2)        = 0.0
      dms_ftpc(1).avg_trkL_ftpc(1)        = 0.0
      dms_ftpc(1).avg_trkL_ftpc(2)        = 0.0
      dms_ftpc(1).res_pad_ftpc(1)         = 0.0
      dms_ftpc(1).res_pad_ftpc(2)         = 0.0
      dms_ftpc(1).res_drf_ftpc(1)         = 0.0
      dms_ftpc(1).res_drf_ftpc(2)         = 0.0

CC:   Loop over FTPC space points
    
      if(fcl_fppoint_h.nok .gt. 0) then
         do i = 1,fcl_fppoint_h.nok
CC:         Determine if space point is in FTPC West
            if (fcl_fppoint(i).row.ge.1 .and. 
     1              fcl_fppoint(i).row.le.10) then 
               iftpc = 1 
CC:         or in FTPC East
            else if (fcl_fppoint(i).row.ge.11 .and. 
     1              fcl_fppoint(i).row.le.20) then
               iftpc = 2
            end if
CC:         Count total number of space points in FTPC West and in FTPC East
            dms_ftpc(1).n_pts_ftpc(iftpc) =
     1         dms_ftpc(1).n_pts_ftpc(iftpc) + 1
CC:         Accumulate total charge deposition in FTPC West and in FTPC East
            dms_ftpc(1).chrg_ftpc_tot(iftpc) =
     1         dms_ftpc(1).chrg_ftpc_tot(iftpc) +
     2                                   fcl_fppoint(i).charge
         end do
      end if


CC:   Loop over FTPC reconstructed tracks

      if(fpt_fptrack_h.nok .gt. 0) then
         nhits(1) = 0
         nhits(2) = 0
         do i = 1,fpt_fptrack_h.nok
CC:        Determine if track is in FTPC West
           do ihit = 1,max_ftpc_hits
            if (fpt_fptrack(i).hitid(ihit) .gt. -1 ) then
              if (fcl_fppoint(fpt_fptrack(i).hitid(ihit)).row .ge. 1 .and.
     1          fcl_fppoint(fpt_fptrack(i).hitid(ihit)).row .le. 10) then
                iftpc = 1
CC:        or in FTPC East
              else if (fcl_fppoint(fpt_fptrack(i).hitid(ihit)).row .ge. 11
     1         .and. fcl_fppoint(fpt_fptrack(i).hitid(ihit)).row .le. 20) then
                  iftpc = 2
              end if
            end if
           end do

CC:        Count total number of reconstructed tracks in FTPC West and East

           dms_ftpc(1).n_trk_ftpc(iftpc) =
     1         dms_ftpc(1).n_trk_ftpc(iftpc) + 1

CC:        Count number of hits used in FTPC West and East

           nhits(iftpc) = nhits(iftpc) + fpt_fptrack(i).nrec

CC:        Accumulate residual, pad direction for FTPC West and East

           dms_ftpc(1).res_pad_ftpc(iftpc) =
     1        dms_ftpc(1).res_pad_ftpc(iftpc) +
     2                                fpt_fptrack(i).chisq(1)

CC:        Accumulate residual, drift direction for FTPC West and East

           dms_ftpc(1).res_drf_ftpc(iftpc) =
     1        dms_ftpc(1).res_drf_ftpc(iftpc) +
     2                                fpt_fptrack(i).chisq(2)

         end do 
 
CC:      Compute fraction of space points used in FTPC West and East

         dms_ftpc(1).hit_frac_ftpc(1) = 
     1       float(nhits(1))/float(dms_ftpc(1).n_pts_ftpc(1))
         dms_ftpc(1).hit_frac_ftpc(2) = 
     1        float(nhits(2))/float(dms_ftpc(1).n_pts_ftpc(2))

CC:      Compute average number of space points per reconstructed track
CC:      used in FTPC West and East

         dms_ftpc(1).avg_trkL_ftpc(1) = 
     1        float(nhits(1))/float(dms_ftpc(1).n_trk_ftpc(1))
         dms_ftpc(1).avg_trkL_ftpc(2) =
     1        float(nhits(2))/float(dms_ftpc(1).n_trk_ftpc(2))

CC:      Compute average residual, pad direction for FTPC West and East

         dms_ftpc(1).res_pad_ftpc(1) =
     1    dms_ftpc(1).res_pad_ftpc(1)/
     2        dms_ftpc(1).n_trk_ftpc(1)
         dms_ftpc(1).res_pad_ftpc(2) =
     1    dms_ftpc(1).res_pad_ftpc(2)/
     2        dms_ftpc(1).n_trk_ftpc(2)

CC:      Compute average residual, drift direction for FTPC West and East

         dms_ftpc(1).res_drf_ftpc(1) =
     1    dms_ftpc(1).res_drf_ftpc(1)
     2        /dms_ftpc(1).n_trk_ftpc(1)
         dms_ftpc(1).res_drf_ftpc(2) =
     1    dms_ftpc(1).res_drf_ftpc(2)
     2       /dms_ftpc(1).n_trk_ftpc(2)

      end if
 

CC:  Fill primary vertex monitor information

CC: prim_vrtx_qual not in any of new monitor_soft tables (LSB)
CC:      do i = 1,10
CC:         dst_monitor_soft(1).prim_vrtx_qual(i) = 0
CC:      end do
      dms_global(1).prim_vrtx_ntrk = 0
      dms_global(1).prim_vrtx_chisq = 0
      do i = 1,6
         dms_global(1).prim_vrtx_cov(i) = 0
      end do

      if(dst_vertex_h.nok .gt. 0) then
         do i = 1,dst_vertex_h.nok
            if(dst_vertex(i).vtx_id .eq. 1) then
CC: this has no analogue in new monitor_soft tables (LSB)
CC:               dst_monitor_soft(1).prim_vrtx_qual(1) =
CC:     1            dst_vertex(i).iflag
CC:               dst_monitor_soft(1).prim_vrtx_qual(2) =
CC:     1            int(dst_vertex(i).pchi2)
               dms_global(1).prim_vrtx_ntrk = 
     1            dst_vertex(i).n_daughters
               dms_global(1).prim_vrtx_chisq =
     1            dst_vertex(i).chisq(1)
               do j = 1,6
                  dms_global(1).prim_vrtx_cov(j) =
     1                dst_vertex(i).covar(j)
               end do
            end if
         end do
      end if
CC:  Successful completion of dms_filler PAM


      DST_MONITOR_SOFT_FILLER=STAFCV_OK
      RETURN
      END
