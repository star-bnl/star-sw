      INTEGER*4 FUNCTION EV0_EVAL2( svt_track_h,  svt_track,
     +                         tpt_track_h,  tpt_track,
     +                         tte_eval_h,  tte_eval,
     +                         vertex_h,  vertex,
     +                         ev0out_h,  ev0out,
     +                         ev0_eval_h, ev0_eval,
     +                         g2t_track_h,  g2t_track,
     +                         g2t_vertex_h,  g2t_vertex)
C>---------------------------------------------------------------------
C   Functional Description : Evaluation of ev0 performance
C   Created  April '97 H.Caines
C   Error conditions	: None-Yet
C>----------------------------------------------------------------------
      IMPLICIT NONE

#include "ev0_eval2.inc"
C---------------

      INTEGER i,j,k,iv1,iv2,iv1s,iv1t,iv2s,iv2t
      INTEGER done1, done2 

      integer ok,start_tpc,start_svt
      integer tls_quick_sort_i
      

*-----executable statements--------------------------------------------
*
* Sort tptrack and svttrack by id_globtrk

      ok = tls_quick_sort_i(tpt_track_h.nok, tpt_track(1).id_globtrk,
     +     tpt_track(2).id_globtrk,tpt_track)
      ok = tls_quick_sort_i(svt_track_h.nok, svt_track(1).id_globtrk,
     +     svt_track(2).id_globtrk, svt_track)

      ev0_eval_h.nok = ev0out_h.nok

      do i=1,tpt_track_h.nok
         if( tpt_track(i).id_globtrk .ne. 0) then
            start_tpc = i
            goto 14
         endif
      enddo
      
 14   continue
      do i=1,svt_track_h.nok
         if( svt_track(i).id_globtrk .ne. 0) then
            start_svt = i
            goto 15
         endif
      enddo
      
 15   continue

      
*
* loop over all found V0
*
      do 10 i=1,ev0out_h.nok
*
* check if daughter particles came from a secondary vertex
*
         iv1=ev0out(i).idneg
         iv2=ev0out(i).idpos
         done1 = 0
         done2 = 0
         iv1s = 0
         iv2s = 0
         iv1t = 0
         iv2t = 0
         
         do j=start_tpc,tpt_track_h.nok
            if( tpt_track(j).id_globtrk .eq. iv1) then
               do k=1,tte_eval_h.nok
                  if( tte_eval(k).rtrk .eq. tpt_track(j).id) then
                     iv1t = tte_eval(k).mtrk
                     done1 = 1
                     goto 13
                  endif
               enddo
            elseif( tpt_track(j).id_globtrk .eq. iv2) then
               do k=1,tte_eval_h.nok
                  if( tte_eval(k).rtrk .eq. tpt_track(j).id) then
                     iv2t = tte_eval(k).mtrk
                     done2 = 1
                     goto 13
                  endif
               enddo
            endif
 13         if ( done1 .eq. 1 .and. done2 .eq. 1) goto 11
         end do
 11      continue
      
         do j=start_svt,svt_track_h.nok
            if( svt_track(j).id_globtrk .eq. iv1
     >           .and. done1 .lt. 10) then
               iv1s = svt_track(j).id_mctrack
               done1 = done1 + 10
            elseif( svt_track(j).id_globtrk .eq. iv2
     >              .and. done2 .lt. 10) then
              iv2s = svt_track(j).id_mctrack
              done2 = done2 + 10
           endif
           if ( done1 .ge. 10 .and. done2 .ge. 10) goto 12
        end do
 12     continue

        iv1 = 0
        iv2 = 0
        if( done1 .eq. 1) then
           iv1 = g2t_track(iv1t).start_vertex_p
           ev0_eval(i).itrue = -1
        elseif( done1 .eq. 10) then
           iv1 = g2t_track(iv1s).start_vertex_p
           ev0_eval(i).itrue  = -2
        else
           if( iv1s .eq. iv1t) then
              iv1 = g2t_track(iv1t).start_vertex_p
              ev0_eval(i).itrue = -3
           else
              iv1 = g2t_track(iv1t).start_vertex_p
              ev0_eval(i).itrue = -4
           endif
        endif

        if( done2 .eq. 1) then
           iv2 = g2t_track(iv2t).start_vertex_p
           ev0_eval(i).itrue = ev0_eval(i).itrue -10
        elseif( done2 .eq. 10) then
           iv2 = g2t_track(iv2s).start_vertex_p
           ev0_eval(i).itrue  = ev0_eval(i).itrue -20
        else
           if( iv2s .eq. iv2t) then
              iv2 = g2t_track(iv2t).start_vertex_p
              ev0_eval(i).itrue = ev0_eval(i).itrue -30
           else
              iv2 = g2t_track(iv2t).start_vertex_p
              ev0_eval(i).itrue = ev0_eval(i).itrue -40
           endif
        endif
              
        IF( (iv1.eq.iv2).and.(iv1.ne.1).and.(iv2.ne.1) ) THEN
         ev0_eval(i).itrue= -1*ev0_eval(i).itrue
         ev0_eval(i).x_true=g2t_vertex(iv1).ge_x(1)
         ev0_eval(i).y_true=g2t_vertex(iv1).ge_x(2)
         ev0_eval(i).z_true=g2t_vertex(iv1).ge_x(3)
         ev0_eval(i).px_true=g2t_track(g2t_vertex(iv1).parent_p).p(1)
         ev0_eval(i).py_true=g2t_track(g2t_vertex(iv1).parent_p).p(2)
         ev0_eval(i).pz_true=g2t_track(g2t_vertex(iv1).parent_p).p(3)
         ev0_eval(i).rapidity_true = 
     +        g2t_track(g2t_vertex(iv1).parent_p).rapidity
        ev0_eval(i).pid_true = 
     +        g2t_track(g2t_vertex(iv1).parent_p).ge_pid

        else
      
         ev0_eval(i).x_true= 0
         ev0_eval(i).y_true= 0
         ev0_eval(i).z_true= 0
         ev0_eval(i).px_true= 0
         ev0_eval(i).py_true= 0
         ev0_eval(i).pz_true= 0
         ev0_eval(i).rapidity_true = 0
        ev0_eval(i).pid_true = 0
         endif

        ev0_eval(i).x = vertex(ev0out(i).id_vertex).x
        ev0_eval(i).y = vertex(ev0out(i).id_vertex).y
        ev0_eval(i).z = vertex(ev0out(i).id_vertex).z
        ev0_eval(i).px = ev0out(i).px
        ev0_eval(i).py = ev0out(i).py
        ev0_eval(i).pz = ev0out(i).pz
        ev0_eval(i).inv_mass_la = ev0out(i).inv_mass_la
        ev0_eval(i).inv_mass_lb = ev0out(i).inv_mass_lb
        ev0_eval(i).inv_mass_k0 = ev0out(i).inv_mass_k0
        ev0_eval(i).rapidity_la = ev0out(i).rapidity_la
        ev0_eval(i).rapidity_lb = ev0out(i).rapidity_lb
        ev0_eval(i).rapidity_k0 = ev0out(i).rapidity_k0
        ev0_eval(i).pid_pos = ev0out(i).pidpos
        ev0_eval(i).pid_neg = ev0out(i).pidneg
        ev0_eval(i).id = ev0out(i).id

        if (iv1t .gt. 0) then
           ev0_eval(i).pid_neg_true = g2t_track(iv1t).ge_pid
        elseif( iv1s .gt. 0) then
           ev0_eval(i).pid_neg_true = g2t_track(iv1s).ge_pid
        else
           ev0_eval(i).pid_neg_true  = 0
        endif
        if( iv2t .gt. 0) then
            ev0_eval(i).pid_pos_true  = g2t_track(iv2t).ge_pid
        elseif( iv2s .gt. 0) then
           ev0_eval(i).pid_pos_true  = g2t_track(iv2s).ge_pid
        else
           ev0_eval(i).pid_pos_true = 0
        endif          

 10     CONTINUE

      ev0_eval2 = stafcv_ok
999   RETURN
      END
 
