CC:>--------------------------------------------------------------------
CC: FILE:       g2t_comb.F.template
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.2 1998/04/28 18:04:27 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION G2T_COMB(
     1         g2t_hit_1_h,         g2t_hit_1 ,
     2       g2t_track_1_h,       g2t_track_1 ,
     3      g2t_vertex_1_h,      g2t_vertex_1 ,
     4         g2t_hit_2_h,         g2t_hit_2 ,
     5       g2t_track_2_h,       g2t_track_2 ,
     6      g2t_vertex_2_h,      g2t_vertex_2 ,
     7       g2t_hit_out_h,       g2t_hit_out ,
     8     g2t_track_out_h,     g2t_track_out ,
     9    g2t_vertex_out_h,    g2t_vertex_out ) 
      IMPLICIT NONE
#include "g2t_comb.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    G2T_COMB
CC: DESCRIPTION:  This routine combines two geant events.  It attempts
CC:               to preserve the pointers among the various tables.
CC: AUTHOR:     David Hardtke (DHHardtke@lbl.gov)
CC: ARGUMENTS:
CC:          IN:
CC:       INOUT:
CC:        g2t_hit_1     - 1st set of g2t tpc hits
CC:      g2t_hit_1_h     - 
CC:      g2t_track_1     - 1st list of g2t tracks
CC:    g2t_track_1_h     - 
CC:     g2t_vertex_1     - 1st list of g2t vertices
CC:   g2t_vertex_1_h     - 
CC:        g2t_hit_2     - 2nd set of g2t tpc hits
CC:      g2t_hit_2_h     - 
CC:      g2t_track_2     - 2nd set of g2t tpc tracks
CC:    g2t_track_2_h     - 
CC:     g2t_vertex_2     - 2nd set of g2t tpc vertices
CC:   g2t_vertex_2_h     - 
CC:      g2t_hit_out     - combined g2t tpc hits
CC:    g2t_hit_out_h     - 
CC:    g2t_track_out     - combined g2t tpc tracks
CC:  g2t_track_out_h     - 
CC:   g2t_vertex_out     - combined g2t tpc vertices
CC:  g2t_vertex_out_h     - 
CC:         OUT:
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------
      integer i,j,idI1
      integer ii,itrk_off,ivtx_off,ihit_off
      data idI1 / 0 /
      save idI1
      character*132 m132
      G2T_COMB=STAFCV_OK
      call Msg_Enable( 'G2T-COMB-I1' )   
    
C now put in first g2t files:
      do i=1,g2t_track_1_h.nok
       if (i.lt.g2t_track_out_h.maxlen) then
        g2t_track_out_h.nok = g2t_track_out_h.nok+1
        g2t_track_out(i).eg_label = g2t_track_1(i).eg_label 
        g2t_track_out(i).eg_pid = g2t_track_1(i).eg_pid 
        g2t_track_out(i).ge_pid = g2t_track_1(i).ge_pid 
        g2t_track_out(i).hit_ctb_p = g2t_track_1(i).hit_ctb_p 
        g2t_track_out(i).hit_eem_p = g2t_track_1(i).hit_eem_p 
        g2t_track_out(i).hit_emc_p = g2t_track_1(i).hit_emc_p 
        g2t_track_out(i).hit_esm_p = g2t_track_1(i).hit_esm_p 
        g2t_track_out(i).hit_ftp_p = g2t_track_1(i).hit_ftp_p 
        g2t_track_out(i).hit_mwc_p = g2t_track_1(i).hit_mwc_p 
        g2t_track_out(i).hit_pgc_p = g2t_track_1(i).hit_pgc_p 
        g2t_track_out(i).hit_psc_p = g2t_track_1(i).hit_psc_p 
        g2t_track_out(i).hit_smd_p = g2t_track_1(i).hit_smd_p 
        g2t_track_out(i).hit_svt_p = g2t_track_1(i).hit_svt_p 
        g2t_track_out(i).hit_tof_p = g2t_track_1(i).hit_tof_p 
        g2t_track_out(i).hit_tpc_p = g2t_track_1(i).hit_tpc_p 
        g2t_track_out(i).hit_vpd_p = g2t_track_1(i).hit_vpd_p 
        g2t_track_out(i).id = g2t_track_1(i).id 
        g2t_track_out(i).is_shower = g2t_track_1(i).is_shower 
        g2t_track_out(i).itrmd_vertex_p = g2t_track_1(i).itrmd_vertex_p 
        g2t_track_out(i).n_ctb_hit = g2t_track_1(i).n_ctb_hit 
        g2t_track_out(i).n_eem_hit = g2t_track_1(i).n_eem_hit 
        g2t_track_out(i).n_emc_hit = g2t_track_1(i).n_emc_hit 
        g2t_track_out(i).n_esm_hit = g2t_track_1(i).n_esm_hit 
        g2t_track_out(i).n_ftp_hit = g2t_track_1(i).n_ftp_hit 
        g2t_track_out(i).n_mwc_hit = g2t_track_1(i).n_mwc_hit 
        g2t_track_out(i).n_pgc_hit = g2t_track_1(i).n_pgc_hit 
        g2t_track_out(i).n_psc_hit = g2t_track_1(i).n_psc_hit 
        g2t_track_out(i).n_smd_hit = g2t_track_1(i).n_smd_hit 
        g2t_track_out(i).n_svt_hit = g2t_track_1(i).n_svt_hit 
        g2t_track_out(i).n_tof_hit = g2t_track_1(i).n_tof_hit 
        g2t_track_out(i).n_tpc_hit = g2t_track_1(i).n_tpc_hit 
        g2t_track_out(i).n_vpd_hit = g2t_track_1(i).n_vpd_hit 
        g2t_track_out(i).next_parent_p = g2t_track_1(i).next_parent_p 
        g2t_track_out(i).next_vtx_trk_p = g2t_track_1(i).next_vtx_trk_p 
        g2t_track_out(i).start_vertex_p = g2t_track_1(i).start_vertex_p 
        g2t_track_out(i).stop_vertex_p = g2t_track_1(i).stop_vertex_p 
        g2t_track_out(i).charge = g2t_track_1(i).charge 
        g2t_track_out(i).e = g2t_track_1(i).e 
        g2t_track_out(i).eta = g2t_track_1(i).eta 
        do j=1,3
         g2t_track_out(i).p(j) = g2t_track_1(i).p(j)
        enddo 
        g2t_track_out(i).pt = g2t_track_1(i).pt 
        g2t_track_out(i).ptot = g2t_track_1(i).ptot 
        g2t_track_out(i).rapidity = g2t_track_1(i).rapidity 
       else
        write(m132,*) 'G2T-COMB-I1 g2t_track_out table too small'
        call Message (m132, 1, idI1 ) 
        G2T_COMB=STAFCV_BAD
        RETURN
       endif
      enddo
C now do vertex
      do i=1,g2t_vertex_1_h.nok
       if (i.lt.g2t_vertex_out_h.maxlen) then
        g2t_vertex_out_h.nok = g2t_vertex_out_h.nok+1
        g2t_vertex_out(i).ge_volume = g2t_vertex_1(i).ge_volume 
        g2t_vertex_out(i).daughter_p = g2t_vertex_1(i).daughter_p 
        g2t_vertex_out(i).eg_label = g2t_vertex_1(i).eg_label 
        g2t_vertex_out(i).eg_proc = g2t_vertex_1(i).eg_proc 
        g2t_vertex_out(i).event_p = g2t_vertex_1(i).event_p 
        g2t_vertex_out(i).ge_medium = g2t_vertex_1(i).ge_medium 
        g2t_vertex_out(i).ge_proc = g2t_vertex_1(i).ge_proc 
        g2t_vertex_out(i).id = g2t_vertex_1(i).id 
        g2t_vertex_out(i).is_itrmd = g2t_vertex_1(i).is_itrmd 
        g2t_vertex_out(i).n_daughter = g2t_vertex_1(i).n_daughter 
        g2t_vertex_out(i).n_parent = g2t_vertex_1(i).n_parent 
        g2t_vertex_out(i).next_itrmd_p = g2t_vertex_1(i).next_itrmd_p 
        g2t_vertex_out(i).next_prim_v_p = g2t_vertex_1(i).next_prim_v_p 
        g2t_vertex_out(i).parent_p = g2t_vertex_1(i).parent_p 
        g2t_vertex_out(i).eg_tof = g2t_vertex_1(i).eg_tof 
        g2t_vertex_out(i).ge_tof = g2t_vertex_1(i).ge_tof 
        do j=1,3
         g2t_vertex_out(i).eg_x(j) = g2t_vertex_1(i).eg_x(j) 
         g2t_vertex_out(i).ge_x(j) = g2t_vertex_1(i).ge_x(j) 
        enddo
       else
        write(m132,*) 'G2T-COMB-I1 g2t_vertex_out table too small'
        call Message (m132, 1, idI1 ) 
        G2T_COMB=STAFCV_BAD
        RETURN
       endif
      enddo
C now do tpc hits
      do i=1,g2t_hit_1_h.nok
       if (i.lt.g2t_hit_out_h.maxlen) then
        g2t_hit_out_h.nok = g2t_hit_out_h.nok+1
        g2t_hit_out(i).id = g2t_hit_1(i).id 
        g2t_hit_out(i).next_tr_hit_p = g2t_hit_1(i).next_tr_hit_p 
        g2t_hit_out(i).track_p = g2t_hit_1(i).track_p 
        g2t_hit_out(i).volume_id = g2t_hit_1(i).volume_id 
        g2t_hit_out(i).de = g2t_hit_1(i).de 
        g2t_hit_out(i).ds = g2t_hit_1(i).ds 
        g2t_hit_out(i).tof = g2t_hit_1(i).tof
        do j=1,3 
         g2t_hit_out(i).p(j) = g2t_hit_1(i).p(j) 
         g2t_hit_out(i).x(j) = g2t_hit_1(i).x(j) 
        enddo
       else
        write(m132,*) 'G2T-COMB-I1 g2t_hit_out table too small'
        call Message (m132, 1, idI1 ) 
        G2T_COMB=STAFCV_BAD
        RETURN
       endif
      enddo
C Now we must add in the second event and keep proper pointers.

      itrk_off = g2t_track_out_h.nok
      ivtx_off = g2t_vertex_out_h.nok
      ihit_off = g2t_hit_out_h.nok

C now put in first g2t files:
      do ii=1,g2t_track_2_h.nok
       i = ii+itrk_off
       if (i.lt.g2t_track_out_h.maxlen) then
        g2t_track_out_h.nok = g2t_track_out_h.nok+1
        g2t_track_out(i).eg_label = g2t_track_2(ii).eg_label+itrk_off 
        g2t_track_out(i).eg_pid = g2t_track_2(ii).eg_pid 
        g2t_track_out(i).ge_pid = g2t_track_2(ii).ge_pid 
        g2t_track_out(i).hit_ctb_p = g2t_track_2(ii).hit_ctb_p 
        g2t_track_out(i).hit_eem_p = g2t_track_2(ii).hit_eem_p 
        g2t_track_out(i).hit_emc_p = g2t_track_2(ii).hit_emc_p 
        g2t_track_out(i).hit_esm_p = g2t_track_2(ii).hit_esm_p 
        g2t_track_out(i).hit_ftp_p = g2t_track_2(ii).hit_ftp_p 
        g2t_track_out(i).hit_mwc_p = g2t_track_2(ii).hit_mwc_p 
        g2t_track_out(i).hit_pgc_p = g2t_track_2(ii).hit_pgc_p 
        g2t_track_out(i).hit_psc_p = g2t_track_2(ii).hit_psc_p 
        g2t_track_out(i).hit_smd_p = g2t_track_2(ii).hit_smd_p 
        g2t_track_out(i).hit_svt_p = g2t_track_2(ii).hit_svt_p 
        g2t_track_out(i).hit_tof_p = g2t_track_2(ii).hit_tof_p 
        g2t_track_out(i).hit_tpc_p = g2t_track_2(ii).hit_tpc_p + ihit_off 
        g2t_track_out(i).hit_vpd_p = g2t_track_2(ii).hit_vpd_p 
        g2t_track_out(i).id = g2t_track_2(ii).id + itrk_off
        g2t_track_out(i).is_shower = g2t_track_2(ii).is_shower 
        g2t_track_out(i).itrmd_vertex_p = g2t_track_2(ii).itrmd_vertex_p+
     +                                    + ivtx_off 
        g2t_track_out(i).n_ctb_hit = g2t_track_2(ii).n_ctb_hit 
        g2t_track_out(i).n_eem_hit = g2t_track_2(ii).n_eem_hit 
        g2t_track_out(i).n_emc_hit = g2t_track_2(ii).n_emc_hit 
        g2t_track_out(i).n_esm_hit = g2t_track_2(ii).n_esm_hit 
        g2t_track_out(i).n_ftp_hit = g2t_track_2(ii).n_ftp_hit 
        g2t_track_out(i).n_mwc_hit = g2t_track_2(ii).n_mwc_hit 
        g2t_track_out(i).n_pgc_hit = g2t_track_2(ii).n_pgc_hit 
        g2t_track_out(i).n_psc_hit = g2t_track_2(ii).n_psc_hit 
        g2t_track_out(i).n_smd_hit = g2t_track_2(ii).n_smd_hit 
        g2t_track_out(i).n_svt_hit = g2t_track_2(ii).n_svt_hit 
        g2t_track_out(i).n_tof_hit = g2t_track_2(ii).n_tof_hit 
        g2t_track_out(i).n_tpc_hit = g2t_track_2(ii).n_tpc_hit 
        g2t_track_out(i).n_vpd_hit = g2t_track_2(ii).n_vpd_hit 
        g2t_track_out(i).next_parent_p = g2t_track_2(ii).next_parent_p+itrk_off 
        g2t_track_out(i).next_vtx_trk_p = g2t_track_2(ii).next_vtx_trk_p
     +                                    + itrk_off 
        g2t_track_out(i).start_vertex_p = g2t_track_2(ii).start_vertex_p
     +                                    + ivtx_off 
        g2t_track_out(i).stop_vertex_p = g2t_track_2(ii).stop_vertex_p
     +                                    + ivtx_off 
        g2t_track_out(i).charge = g2t_track_2(ii).charge 
        g2t_track_out(i).e = g2t_track_2(ii).e 
        g2t_track_out(i).eta = g2t_track_2(ii).eta 
        do j=1,3
         g2t_track_out(i).p(j) = g2t_track_2(ii).p(j)
        enddo 
        g2t_track_out(i).pt = g2t_track_2(ii).pt 
        g2t_track_out(i).ptot = g2t_track_2(ii).ptot 
        g2t_track_out(i).rapidity = g2t_track_2(ii).rapidity 
       else
        write(m132,*) 'G2T-COMB-I1 g2t_track_out table too small'
        call Message (m132, 1, idI1 ) 
        G2T_COMB=STAFCV_BAD
        RETURN
       endif
      enddo
C now do vertex
      do ii=1,g2t_vertex_2_h.nok
       i = ii+ivtx_off
       if (i.lt.g2t_vertex_out_h.maxlen) then
        g2t_vertex_out_h.nok = g2t_vertex_out_h.nok + 1
        g2t_vertex_out(i).ge_volume = g2t_vertex_2(ii).ge_volume 
        g2t_vertex_out(i).daughter_p = g2t_vertex_2(ii).daughter_p + itrk_off 
        g2t_vertex_out(i).eg_label = g2t_vertex_2(ii).eg_label 
        g2t_vertex_out(i).eg_proc = g2t_vertex_2(ii).eg_proc 
        g2t_vertex_out(i).event_p = g2t_vertex_2(ii).event_p 
        g2t_vertex_out(i).ge_medium = g2t_vertex_2(ii).ge_medium 
        g2t_vertex_out(i).ge_proc = g2t_vertex_2(ii).ge_proc 
        g2t_vertex_out(i).id = g2t_vertex_2(ii).id + ivtx_off
        g2t_vertex_out(i).is_itrmd = g2t_vertex_2(ii).is_itrmd 
        g2t_vertex_out(i).n_daughter = g2t_vertex_2(ii).n_daughter 
        g2t_vertex_out(i).n_parent = g2t_vertex_2(ii).n_parent 
        g2t_vertex_out(i).next_itrmd_p = g2t_vertex_2(ii).next_itrmd_p 
     +                                   + ivtx_off 
        g2t_vertex_out(i).next_prim_v_p = g2t_vertex_2(ii).next_prim_v_p +
     +                                    ivtx_off 
        g2t_vertex_out(i).parent_p = g2t_vertex_2(ii).parent_p 
        g2t_vertex_out(i).eg_tof = g2t_vertex_2(ii).eg_tof 
        g2t_vertex_out(i).ge_tof = g2t_vertex_2(ii).ge_tof 
        do j=1,3
         g2t_vertex_out(i).eg_x(j) = g2t_vertex_2(ii).eg_x(j) 
         g2t_vertex_out(i).ge_x(j) = g2t_vertex_2(ii).ge_x(j) 
        enddo
       else
        write(m132,*) 'G2T-COMB-I1 g2t_vertex_out table too small'
        call Message (m132, 1, idI1 ) 
        G2T_COMB=STAFCV_BAD
        RETURN
       endif
      enddo
C now do tpc hits
      do ii=1,g2t_hit_2_h.nok
       i = ii+ihit_off
       if (i.lt.g2t_hit_out_h.maxlen) then
        g2t_hit_out_h.nok = g2t_hit_out_h.nok + 1
        g2t_hit_out(i).id = g2t_hit_2(ii).id  + ihit_off
         if (g2t_hit_2(ii).next_tr_hit_p.ne.0) then
          g2t_hit_out(i).next_tr_hit_p = g2t_hit_2(ii).next_tr_hit_p 
     +                                     + ihit_off
         else
          g2t_hit_out(i).next_tr_hit_p = 0
         endif
        g2t_hit_out(i).track_p = g2t_hit_2(ii).track_p + itrk_off 
        g2t_hit_out(i).volume_id = g2t_hit_2(ii).volume_id 
        g2t_hit_out(i).de = g2t_hit_2(ii).de 
        g2t_hit_out(i).ds = g2t_hit_2(ii).ds 
        g2t_hit_out(i).tof = g2t_hit_2(ii).tof
        do j=1,3 
         g2t_hit_out(i).p(j) = g2t_hit_2(ii).p(j) 
         g2t_hit_out(i).x(j) = g2t_hit_2(ii).x(j) 
        enddo
       else
        write(m132,*) 'G2T-COMB-I1 g2t_hit_out table too small'
        call Message (m132, 1, idI1 ) 
        G2T_COMB=STAFCV_BAD
        RETURN
       endif
      enddo

      RETURN
      END
