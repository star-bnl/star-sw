      subroutine find_parent_b(
     +     ems_control_h, ems_control,
     +     g2t_vertex_h, g2t_vertex,
     +     g2t_track_h,  g2t_track,
     +     itrk, ipartrk)

      IMPLICIT NONE
#include "PAM.inc"
#include "ems_control.inc"
#include "g2t_vertex.inc"
#include "g2t_track.inc"
      RECORD / table_head_st        / ems_control_h
      RECORD / ems_control_st       / ems_control(*)
      RECORD / table_head_st        / g2t_vertex_h
      RECORD / g2t_vertex_st        / g2t_vertex(*)
      RECORD / table_head_st        / g2t_track_h
      RECORD / g2t_track_st         / g2t_track(*)
      integer k,itrk,ipartrk,iparvtx
      real parent_vert_rho

      k  = 0
      ipartrk=itrk
      iparvtx=g2t_track(itrk).start_vertex_p
 100  Continue
      k  = k + 1
      parent_vert_rho = MAX(
     1     SQRT(g2t_vertex(iparvtx).ge_x(1)**2   ! parent has geant ID
     1     +    g2t_vertex(iparvtx).ge_x(2)**2), 
     1     SQRT(g2t_vertex(iparvtx).eg_x(1)**2   ! parent has generator ID
     1     +    g2t_vertex(iparvtx).eg_x(2)**2)) 
      if(parent_vert_rho.gt.ems_control(1).bemc_inner_r)then
         if(g2t_vertex(iparvtx).n_parent.gt.0)then
            ipartrk  = g2t_vertex(iparvtx).parent_p
            iparvtx  = g2t_track(ipartrk).start_vertex_p 
            if(k.gt.50)then
               write(*,*)
     1           '***find_parent_b: too many parent vertices!! ',k,itrk
            endif
            goto 100
         else
            write(*,*)'***find_parent_b: Lost vertex outside of BEMC' 
         endif
        endif
       END 











