CC:>--------------------------------------------------------------------
CC: FILE:       g2t_comb.F
CC: HISTORY:
CC:             apr 98-v000a - David Hardtke 
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
CC: DESCRIPTION:   This routine combines two geant events.  It attempts
CC:                to preserve the pointers among the various tables.
CC: AUTHOR:        David Hardtke (DHHardtke@lbl.gov)
CC: Modifications: PN,1.11.99  - simplify a bit
CC: ARGUMENTS:
CC:          IN:
CC:        g2t_hit_1(_h)     - 1st set of g2t tpc hits
CC:      g2t_track_1(_h)     - 1st list of g2t tracks
CC:     g2t_vertex_1(_h)     - 1st list of g2t vertices
CC:        g2t_hit_2(_h)     - 2nd set of g2t tpc hits
CC:      g2t_track_2(_h)     - 2nd set of g2t tpc tracks
CC:     g2t_vertex_2(_h)     - 2nd set of g2t tpc vertices
CC:         OUT:
CC:      g2t_hit_out(_h)     - combined g2t tpc hits
CC:    g2t_track_out(_h)     - combined g2t tpc tracks
CC:   g2t_vertex_out(_h)     - combined g2t tpc vertices
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------
      integer    i,j,nv,nt,nh,itrk_off,ivtx_off,ihit_off
      integer    idI1 / 0 /
      character  m132*132,what*12

      call Msg_Enable( 'G2T-COMB-I1' )   

      what = 'g2t_vertex_out'
      nv   =  g2t_vertex_1_h.nok+g2t_vertex_2_h.nok
      if (nv .gt. g2t_vertex_out_h.maxlen) goto 999

      what = 'g2t_track_out'
      nt   =  g2t_track_1_h.nok + g2t_track_2_h.nok
      if (nt .gt. g2t_track_out_h.maxlen)  goto 999

      what = 'g2t_hit_out'
      nh   =  g2t_hit_1_h.nok   +   g2t_hit_2_h.nok
      if (nh .gt. g2t_track_out_h.maxlen)  goto 999

C put in first g2t event, add in the second event and keep proper pointers.

      ivtx_off = g2t_vertex_out_h.nok
      itrk_off = g2t_track_out_h.nok
      ihit_off = g2t_hit_out_h.nok

C now do vertex
      do i = 1,g2t_vertex_1_h.nok
        g2t_vertex_out(i)              = g2t_vertex_1(i)
      enddo
      do j = 1,g2t_vertex_2_h.nok
        i = j+ivtx_off
        g2t_vertex_out(i)              = g2t_vertex_2(j)
        g2t_vertex_out(i).id           = g2t_vertex_2(j).id           +ivtx_off
        g2t_vertex_out(i).daughter_p   = g2t_vertex_2(j).daughter_p   +itrk_off
        g2t_vertex_out(i).next_itrmd_p = g2t_vertex_2(j).next_itrmd_p +ivtx_off
        g2t_vertex_out(i).next_prim_v_p= g2t_vertex_2(j).next_prim_v_p+ivtx_off
      enddo

C now do tracks
      do i = 1,g2t_track_1_h.nok
        g2t_track_out(i)               = g2t_track_1(i)
      enddo
      do j = 1,g2t_track_2_h.nok
        i = j+itrk_off
        g2t_track_out(i)               = g2t_track_2(j)
        g2t_track_out(i).id            = g2t_track_2(j).id            +itrk_off
        g2t_track_out(i).eg_label      = g2t_track_2(j).eg_label      +itrk_off
        g2t_track_out(i).hit_tpc_p     = g2t_track_2(j).hit_tpc_p     +ihit_off
        g2t_track_out(i).itrmd_vertex_p= g2t_track_2(j).itrmd_vertex_p+ivtx_off
        g2t_track_out(i).next_parent_p = g2t_track_2(j).next_parent_p +itrk_off
        g2t_track_out(i).next_vtx_trk_p= g2t_track_2(j).next_vtx_trk_p+itrk_off
        g2t_track_out(i).start_vertex_p= g2t_track_2(j).start_vertex_p+ivtx_off
        g2t_track_out(i).stop_vertex_p = g2t_track_2(j).stop_vertex_p +ivtx_off
      enddo

C now do tpc hits
      do i = 1,g2t_hit_1_h.nok
        g2t_hit_out(i)         = g2t_hit_1(i)
      enddo
      do j = 1,g2t_hit_2_h.nok
        i = j+ihit_off
        g2t_hit_out(i)         = g2t_hit_2(j)
        g2t_hit_out(i).id      = g2t_hit_2(j).id            + ihit_off
        if (g2t_hit_2(j).next_tr_hit_p.ne.0) g2t_hit_out(i).next_tr_hit_p =
     >                           g2t_hit_2(j).next_tr_hit_p + ihit_off
        g2t_hit_out(i).track_p = g2t_hit_2(j).track_p       + itrk_off 
      enddo

      g2t_vertex_out_h.nok = nv
      g2t_track_out_h.nok  = nt
      g2t_hit_out_h.nok    = nh
      G2T_COMB             = STAFCV_OK
      RETURN

 999  write(m132,*) 'G2T-COMB-I1 '//what//' table too small'
      call Message (m132, 1, idI1 ) 
      g2t_vertex_out_h.nok = 0
      g2t_track_out_h.nok  = 0
      g2t_hit_out_h.nok    = 0
      G2T_COMB             = STAFCV_BAD
      RETURN
      END
