
      INTEGER*4 FUNCTION EST_EVAL(
     1         g2t_track_h,         g2t_track ,
     2           tptrack_h,           tptrack ,
     3          ttemctrk_h,          ttemctrk ,
     4          est_ctrl_h,          est_ctrl ,
     5         est_match_h,         est_match ,
     6            est_ev_h,            est_ev ,
     7           scs_spt_h,           scs_spt) 
      IMPLICIT NONE
#include "est_eval.inc"
      
      integer i,j,max_track, max_id
      parameter (max_track= 40000)
      parameter (max_id = 20)
      integer key(max_track,max_id)
      real dens_si, dens_p10
      parameter (dens_si = 2.33)
      parameter (dens_p10= 1.78E-3) 
      logical led

c-----------------------------------------------------------------------------
c ng = nbre de points dans scs_spt
c nf = nbre de points trouves par est
c nc = nbre de points corrects
c status = -1 trace eteinte (si nseg#1--> au moins 1 segment est pris en compte
c                                        les autres sont eteints)
c status = 1X (1 segment) 2X (segments>1)
c status = 11/21 ng=0 nf#0
c status = 12/22 ng#0, nf=0
c status = 13/23 ng#0, nf#0 et nc#nf
c status = 100/200 ng#0, nf#0 et nc=nf (correctes)
c-----------------------------------------------------------------------------

c     ****initialization***
      
      est_ev_h.nok = est_match_h.nok
      
      do i =1 ,est_match_h.nok
         do j=1,10
            est_ev(i).id_mctrsvt(j) = 0
            est_ev(i).svt_h(j)      = 0
            est_ev(i).perf_h(j)     = 0
            est_ev(i).perf_de(j)    = 0.
            est_ev(i).svt_de(j)     = 0.
         enddo
         est_ev(i).tpc_dedx(1)      = 0.
         est_ev(i).tpc_dedx(2)      = 0.
         est_ev(i).id               = est_match(i).id
         est_ev(i).idtpc            = est_match(i).id_tpctrack
         est_ev(i).nrec             = tptrack(i).nrec
         est_ev(i).tpc_dedx(1)      = tptrack(i).dedx(1)/dens_p10
         est_ev(i).tpc_dedx(2)      = tptrack(i).dedx(2)/dens_p10
         
         do j=1,10
            est_ev(i).id_mctrsvt(j) = est_match(i).id_mctrsvt(j)
            est_ev(i).svt_h(j)      = est_match(i).id_hit(j)
            est_ev(i).perf_h(j)     = est_match(i).perf_scs_id(j)
            est_ev(i).id_det(j)     = est_match(i).id_det(j)
         enddo
         do j=1,10
            if (est_ev(i).svt_h(j).gt.0) est_ev(i).svt_de(j)=scs_spt(est_ev(i).svt_h(j)).de(1)/dens_si
            if (est_ev(i).perf_h(j).gt.0) est_ev(i).perf_de(j)=scs_spt(est_ev(i).perf_h(j)).de(1)/dens_si
         enddo
         


         est_ev(i).id_mctrtpc = est_match(i).id_mctrtpc
         est_ev(i).ng = est_match(i).perf_svt_hits    ! sortie de scs_spt
         est_ev(i).nf = est_match(i).n_svt_hits       ! nbre de points trouves
         est_ev(i).nc = 0                             ! nbre de points corrects
         est_ev(i).r0 = est_match(i).r0
         est_ev(i).phi0 = est_match(i).phi0
         est_ev(i).psi = est_match(i).psi
         est_ev(i).layer = est_match(i).layer
         est_ev(i).chisq(1)=tptrack(i).chisq(1)
         est_ev(i).chisq(2)=tptrack(i).chisq(2)
         est_ev(i).r0_old=tptrack(i).r0
         est_ev(i).z0_old=tptrack(i).z0
         est_ev(i).z0=est_match(i).z0
         est_ev(i).tanl_old = tptrack(i).tanl
         est_ev(i).q = est_match(i).q
         est_ev(i).binpt = est_match(i).binpt
         do j = 1, est_ev(i).nf
            if (est_ev(i).id_mctrsvt(j).eq.est_ev(i).id_mctrtpc) then 
               est_ev(i).nc = est_ev(i).nc + 1
            endif
         enddo
         est_ev(i).nseg = 0
         est_ev(i).tanl = est_match(i).tanl
         est_ev(i).pid  = est_match(i).pid
         est_ev(i).t2flag = est_match(i).flag
         est_ev(i).invpt   = est_match(i).invp_tpc
         est_ev(i).ginvpt  = est_match(i).invp
         est_ev(i).status  = 0
         est_ev(i).sec     = est_match(i).sec
         est_ev(i).comp    = 0
      enddo

c     get a key between id_mctrack and id_tpc_track
      do i=1, g2t_track_h.nok
         do j=1,max_id
            key(i,j)=0
         enddo
      enddo
      
      do i=1, est_match_h.nok
         j=1
         do while (key(est_match(i).id_mctrtpc,j).ne.0.and.j.le.max_id)
            j=j+1
         enddo
         if (j.le.max_id) then
            key(est_match(i).id_mctrtpc,j)=est_match(i).id
         else
            est_ev(i).nseg = 21
            est_ev(i).status = -1
         endif
      enddo
     

 
      do i=1, est_match_h.nok
         j=1
         led=.false.
         do while (key(est_match(i).id_mctrtpc,j).ne.0.and.j.le.max_id)
            j=j+1
         enddo
         if (est_ev(i).nseg.eq.0) est_ev(i).nseg = j-1             
         
         
         if (est_ev(i).nseg.eq.1) then 
            if (est_ev(i).ng.eq.0) then
               if (est_ev(i).nf.eq.0) then
                  est_ev(i).status = 10 !  no svt hits and no hits found --> normal
               else 
                  est_ev(i).status = 11 ! no svt hits but some hits found --> ghost
               endif
            else ! ng.ne.0
               if (est_ev(i).nf.eq.0) then
                  est_ev(i).status = 12 !  hits and no hits found --> pas de match
               else 
                  if (est_ev(i).nc.eq.est_ev(i).nf) then
                     est_ev(i).status = 100 ! found hits are correct 
                  else
                     est_ev(i).status = 13 ! found hits are not all correct --> ghost
                  endif
               endif
            endif   
         else
            if (est_ev(i).status.ne.-1) then
               if (est_ev(i).ng.eq.0) then
                  if (est_ev(i).nf.eq.0) then
                     do j = 1, est_ev(i).nseg
                        if (est_ev(key(est_match(i).id_mctrtpc,j)).nf.ne.0) then
                           est_ev(key(est_match(i).id_mctrtpc,j)).status= 21 !no hits,no hits found 
                           led = .true.
                        else
                           est_ev(key(est_match(i).id_mctrtpc,j)).status=-1
                        endif
                     enddo
                     if (.not.led) then
                        est_ev(key(est_match(i).id_mctrtpc,1)).status= 20 ! no hits, but hits found
                     endif
                  else ! nf.ne.0
                     do j = 1 , est_ev(i).nseg
                        if (est_ev(key(est_match(i).id_mctrtpc,j)).nf.ne.0) then
                           est_ev(key(est_match(i).id_mctrtpc,j)).status=21  ! no hits, but hits found  
                        else
                           est_ev(key(est_match(i).id_mctrtpc,j)).status=-1
                        endif
                     enddo
                  endif
               else ! ng.ne.0
                  if (est_ev(i).nf.eq.0) then
                     do j = 1, est_ev(i).nseg
                        if (est_ev(key(est_match(i).id_mctrtpc,j)).nf.ne.0) then
                           if (est_ev(key(est_match(i).id_mctrtpc,j)).nf.eq.
     >                          est_ev(key(est_match(i).id_mctrtpc,j)).nc) then
                              est_ev(key(est_match(i).id_mctrtpc,j)).status=200  ! hits and hits found
                           else
                              est_ev(key(est_match(i).id_mctrtpc,j)).status=23  ! uncorrect hits found 
                           endif
                           led = .true.
                        else ! nf.eq.0
                           est_ev(key(est_match(i).id_mctrtpc,j)).status=-1
                        endif
                     enddo
                     if (.not.led) then
                        est_ev(key(est_match(i).id_mctrtpc,1)).status=22  
                     endif
                  else  ! nf.ne.0
                     do j=1, est_ev(i).nseg
                        if (est_ev(key(est_match(i).id_mctrtpc,j)).nf.eq.0) then
                           est_ev(key(est_match(i).id_mctrtpc,j)).status=-1
                        else
                           if (est_ev(key(est_match(i).id_mctrtpc,j)).nf.eq.
     >                          est_ev(key(est_match(i).id_mctrtpc,j)).nc) then
                              est_ev(key(est_match(i).id_mctrtpc,j)).status=200 ! correct
                           else
                              est_ev(key(est_match(i).id_mctrtpc,j)).status=23 ! ghost
                           endif
                        endif
                     enddo
                  endif
               endif
            endif               ! status -1
         endif                  ! nseg
      enddo
      
      RETURN
      END
