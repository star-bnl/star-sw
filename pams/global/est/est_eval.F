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
#include "est_param_def.inc"
    
      integer i,j,maxtrack,max_id,index
      integer max_tpc_track,mcidtorow
      integer malloc
      
C      parameter (maxtrack= 40000)
      parameter (max_id = 20)
C      parameter (max_tpc_track=5000)
      
      dimension mcidtorow(1)
      integer key(max_id,1)
      real dens_si, dens_p10
      parameter (dens_si = 2.33)
      parameter (dens_p10= 1.78E-3) 
      logical led

      pointer(mcidtorow_p,mcidtorow)
      pointer(key_p,key)
c-----------------------------------------------------------------------------
c ng = nbre de points dans scs_spt
c nf = nbre de points trouves par est
c nc = nbre de points corrects
c status = -1 trace eteinte (si nseg#1--> au moins 1 segment est pris en compte
c                                        les autres sont eteints)
c status = 1X (1 segment) 2X (segments>1)
c status = 10/20 ng=0 nf=0
c status = 11/21 ng=0 nf#0
c status = 12/22 ng#0, nf=0
c status = 13/23 ng#0, nf#0 et nc#nf
c status = 100/200 ng#0, nf#0 et nc=nf (correctes)
c-----------------------------------------------------------------------------

c     ****initialization***
C define and allocate memory for the mcidtorow array
C there is one row per mcid of the tpc tracks
      maxtrack=g2t_track_h.nok
      mcidtorow_p=malloc(maxtrack*4)
      call vzero(mcidtorow,maxtrack)
      write(6,*)'est_eval : MAXTRACK=',maxtrack

      est_ev_h.nok = est_match_h.nok
      
      do i =1 ,est_match_h.nok
         do j=1,NUM_SVT_HIT
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
         
         do j=1,NUM_SVT_HIT
            est_ev(i).id_mctrsvt(j) = est_match(i).id_mctrsvt(j)
            est_ev(i).svt_h(j)      = est_match(i).id_hit(j)
            est_ev(i).perf_h(j)     = est_match(i).perf_scs_id(j)
            est_ev(i).id_det(j)     = est_match(i).id_det(j)
            if (est_ev(i).svt_h(j).gt.0) 
     *         est_ev(i).svt_de(j)=scs_spt(est_ev(i).svt_h(j)).de(1)/dens_si
            if (est_ev(i).perf_h(j).gt.0) 
     *         est_ev(i).perf_de(j)=scs_spt(est_ev(i).perf_h(j)).de(1)/dens_si
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

      j=0
      do i=1,est_match_h.nok
         if (est_match(i).id_mctrtpc.ne.0) then
            if (mcidtorow(est_match(i).id_mctrtpc).eq.0) then
               j=j+1
               mcidtorow(est_match(i).id_mctrtpc)=j
            endif
         endif
      enddo
      max_tpc_track=j
      write(6,*)'found ',j,' tracks from the tpc'
C define and set the key table 
C up to max_id tpc segments of a mc tracks are allowed
      key_p=malloc(max_tpc_track*max_id*4)
      call vzero(key,max_tpc_track*max_id)

      do i=1, est_match_h.nok
         if (est_match(i).id_mctrtpc.ne.0) then
            j=1
            index=mcidtorow(est_match(i).id_mctrtpc)
            do while (key(j,index).ne.0.and.j.le.max_id)
               j=j+1
            enddo
            if (j.le.max_id) then
               key(j,index)=est_match(i).id
            else
               est_ev(i).nseg = 21
               est_ev(i).status = -1
            endif
         else
            est_ev(i).status=-1
         endif
      enddo
     
      do i=1, est_match_h.nok
         if (est_match(i).id_mctrtpc.ne.0) then
            j=1
            led=.false.
            index=mcidtorow(est_match(i).id_mctrtpc)
            do while (key(j,index).ne.0.and.j.le.max_id)
               j=j+1
            enddo
            if (est_ev(i).nseg.eq.0) est_ev(i).nseg = j-1             
            
            
            if (est_ev(i).nseg.eq.1) then 
               if (est_ev(i).ng.eq.0) then
                  if (est_ev(i).nf.eq.0) then
                     est_ev(i).status = 10 !no svt hts and no hts fnd --> normal
                  else 
                     est_ev(i).status = 11 ! no svt hits but some hits found --> ghost
                  endif
               else             ! ng.ne.0
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
                           if (est_ev(key(j,index)).nf.ne.0) then
                              est_ev(key(j,index)).status= 21 !no hits,no hits found 
                              led = .true.
                           else
                              est_ev(key(j,index)).status=-1
                           endif
                        enddo
                        if (.not.led) then
                           est_ev(key(1,index)).status= 20 ! no hits, but hits found
                        endif
                     else       ! nf.ne.0
                        do j = 1 , est_ev(i).nseg
                           if (est_ev(key(j,index)).nf.ne.0) then
                              est_ev(key(j,index)).status=21 ! no hits, but hits found  
                           else
                              est_ev(key(j,index)).status=-1
                           endif
                        enddo
                     endif
                  else          ! ng.ne.0
                     if (est_ev(i).nf.eq.0) then
                        do j = 1, est_ev(i).nseg
                           if (est_ev(key(j,index)).nf.ne.0) then
                              if (est_ev(key(j,index)).nf.eq.
     >                             est_ev(key(j,index)).nc) then
                                 est_ev(key(j,index)).status=200 ! hits and hits found
                              else
                                 est_ev(key(j,index)).status=23 ! uncorrect hits found 
                              endif
                              led = .true.
                           else ! nf.eq.0
                              est_ev(key(j,index)).status=-1
                           endif
                        enddo
                        if (.not.led) then
                           est_ev(key(1,index)).status=22  
                        endif
                     else       ! nf.ne.0
                        do j=1, est_ev(i).nseg
                           if (est_ev(key(j,index)).nf.eq.0) then
                              est_ev(key(j,index)).status=-1
                           else
                              if (est_ev(key(j,index)).nf.eq.
     >                             est_ev(key(j,index)).nc) then
                                 est_ev(key(j,index)).status=200 ! correct
                              else
                                 est_ev(key(j,index)).status=23 ! ghost
                              endif
                           endif
                        enddo
                     endif
                  endif
               endif            ! status -1
            endif               ! nseg
         endif 
      enddo
      
      call free(mcidtorow_p)
      call free(key_p)

      RETURN
      END
