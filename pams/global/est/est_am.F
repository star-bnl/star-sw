      integer FUNCTION est_am(
     >     geom_h, geom, 
     >     shape_h, shape,
     >     activea_h, activea,
     >     srspar_h, srspar,
     >     config_h, config, 
     >     scs_spt_h,scs_spt,
     >     tphit_h, tphit,
     >     tptrack_h, tptrack, 
     >     eval_h, eval,
     >     ttemctrk_h, ttemctrk,
     >     est_ctrl_h, est_ctrl,
     >     est_match_h, est_match,
     >     egrpar_h,egrpar)
      implicit none

CCC   Author: PL,WP,LM,CR Sun Nov 23 14:00:37 MET 1997
CCC   Module est_am
CCC   Function makes a tracking from TPC to SVT

#include "est_am.inc"
#include "est_param_def.inc"

      integer i,ii,j,llay
      integer jj, pl,pll, pl4 ,pas, est_erasor,iret,nkeep,nremove
      integer waf_min(4),waf_max(4),maxtrack,maxid,key,nhit
      parameter (maxtrack=40000)
      parameter (maxid=20)
      
      dimension key(maxtrack,maxid)
      DATA waf_min /1000, 3000, 5000, 7000/
      DATA waf_max /2999, 4999, 6999, 8999/

c-----------------------------------------------------------------

      write(6,*)'>>>>> Tracking options :'
      write(6,11)est_ctrl(1).maxloop,' loop(s) over the TPC pt'
 11   format(i3,a24)
      write(6,12)' From layer ',est_ctrl(1).maxlay,' to layer ',
     +     est_ctrl(1).minlay
 12   format(a11,i1,a10,i1)
      est_match_h.nok = tptrack_h.nok


c     Initialization

      do i=1,scs_spt_h.nok
         scs_spt(i).flag = 0
      enddo
      do i=1,maxtrack
         do j=1,maxid
            key(i,j)=0
         enddo
      enddo
      do i=1,scs_spt_h.nok
         j=1
         do while (key(scs_spt(i).id_mctrack,j).ne.0 .and. j.le.maxid)
            j=j+1
         enddo
         if(j.le.maxid) key(scs_spt(i).id_mctrack,j)=scs_spt(i).id
      enddo
         
     
      
      do i=1,est_match_h.nok
         est_match(i).id = i
         est_match(i).id_tpctrack = tptrack(i).id
         est_match(i).n_svt_hits = 0
         est_match(i).id_mctrtpc = 0
         est_match(i).flag=-1
         est_match(i).sec=0
         est_match(i).pid=0

         do jj=1,NUM_SVT_HIT
            est_match(i).id_det(jj) = 0
            est_match(i).id_hit(jj) = 0
            est_match(i).z_4th(jj) = 0.
            est_match(i).phi_4th(jj) = 0.
            est_match(i).r_4th(jj) = 0.
            est_match(i).id_mctrsvt(jj) = 0
            est_match(i).ang_det(jj) = 0.
            est_match(i).rflag(jj) = 0
            est_match(i).z_err(jj) = 0.
            est_match(i).phi_err(jj) = 0.
            est_match(i).perf_svt_hits = 0
            est_match(i).perf_scs_id(jj)=0
            est_match(i).perf_svt_waf(jj)=0
            est_match(i).perf_svt_x(jj)=0.
            est_match(i).perf_svt_y(jj)=0.
            est_match(i).perf_svt_z(jj)=0.
            est_match(i).perf_proj_x(jj)=0.
            est_match(i).perf_proj_y(jj)=0.
            est_match(i).perf_proj_z(jj)=0.
            est_match(i).layer = est_ctrl(1).maxlay+1
         enddo

C We get additionnal info on the tpc track : id_mctrack, pid and vid
         j=1
         jj=0
         do while (j.le.eval_h.nok.and.jj.eq.0) 
            if (eval(j).rtrk.eq.tptrack(i).id) then
               est_match(i).id_mctrtpc = eval(j).mtrk
               jj=1
            endif
            j=j+1
         enddo
         j=1
         jj=0
         do while (j.le.ttemctrk_h.nok.and.jj.eq.0)
            if (ttemctrk(j).mcid.eq.est_match(i).id_mctrtpc) then
               est_match(i).pid = ttemctrk(j).pid
               est_match(i).sec = ttemctrk(j).vid
               jj=1
            endif
            j=j+1
         enddo

C We get the track parameters from the tptrack table
         est_match(i).q = tptrack(i).q
         est_match(i).invp = tptrack(i).invp
         est_match(i).invp_tpc = tptrack(i).invp ! original 
         est_match(i).phi0 = tptrack(i).phi0
         est_match(i).psi = tptrack(i).psi
         est_match(i).r0 = tptrack(i).r0
         est_match(i).tanl = tptrack(i).tanl
         est_match(i).z0 = tptrack(i).z0

      enddo

      if (est_ctrl(1).ktrl1.eq.1) then   
         write(6,*)'********** START GET SVT HITS ************'
      endif

      do ii=1,est_match_h.nok
         do j=est_ctrl(1).maxlay,est_ctrl(1).minlay,-1 ! loop over 4 layers
            do jj=1,maxid
               if (scs_spt(key(est_match(ii).id_mctrtpc,jj)).id_wafer
     +              .ge.waf_min(j).and.
     >              scs_spt(key(est_match(ii).id_mctrtpc,jj)).id_wafer
     +              .le.waf_max(j)) then
                  nhit=est_match(ii).perf_svt_hits + 1
                  if (nhit.eq.11) then
                     est_match(ii).perf_svt_hits=nhit
                     goto 111
                  endif
                  est_match(ii).perf_scs_id(nhit) = 
     +                 key(est_match(ii).id_mctrtpc,jj)
                  est_match(ii).perf_svt_waf(nhit) =
     +               scs_spt(key(est_match(ii).id_mctrtpc,jj)).id_wafer
                  est_match(ii).perf_svt_x(nhit) = 
     +                 scs_spt(key(est_match(ii).id_mctrtpc,jj)).x(1)
                  est_match(ii).perf_svt_y(nhit) =
     +                 scs_spt(key(est_match(ii).id_mctrtpc,jj)).x(2)
                  est_match(ii).perf_svt_z(nhit) = 
     +                 scs_spt(key(est_match(ii).id_mctrtpc,jj)).x(3)
                  est_match(ii).perf_svt_hits=nhit
               endif
            enddo
         enddo
 111     continue
      enddo
      if (est_ctrl(1).ktrl1.eq.1) then
         write(6,*)'********** END GET SVT HITS ************'
      endif
         
C We flag bad TPC tracks (pt cut, min and max hit number and pid)
      if (est_ctrl(1).ktrl1.eq.1) then
         write(6,*)'********** START FLAG BAD TRACKS ************' 
      endif
         do i=1,est_match_h.nok
         if ((1./est_match(i).invp).lt.est_ctrl(1).ptcut) 
     +           est_match(i).flag=2
         if ((tptrack(i).nrec.le.est_ctrl(1).min_nrec)
     >        .or.(tptrack(i).nrec.ge.est_ctrl(1).max_nrec)) 
     +        est_match(i).flag=3
         if(est_match(i).pid.le.est_ctrl(1).pid_cut) est_match(i).flag=5
         if(abs(est_match(i).tanl).ge.est_ctrl(1).tanl_cut)
     +        est_match(i).flag=6
         if (tptrack(i).flag.lt.0) est_match(i).flag=7
         if (est_match(i).perf_svt_hits.ge.11) est_match(i).flag=8         
      enddo 
      if (est_ctrl(1).ktrl1.eq.1) then
         write(6,*)'********** END FLAG BAD TRACKS ************' 
         write(6,*)'****** PREREFIT SPECIAL LOOP ******'
      endif

c     TPC tracks are refitted with a max number of hits depending on pt
      do pl=1,est_ctrl(1).maxloop       
         est_ctrl(1).current_loop=pl
         if (est_ctrl(1).prerefit(pl).ne.-1) then
            CALL est_prerefit2(tphit_h, tphit,
     >           tptrack_h, tptrack, 
     >           est_ctrl_h, est_ctrl,
     >           est_match_h, est_match,
     >           egrpar_h,egrpar)
            
         endif
      enddo
      
      if (est_ctrl(1).ktrl1.eq.1) then
         write(6,*)'*** END OF PREREFIT SPECIAL LOOP ***'
      endif

c     Starting the tracking
c     5 passes are done with the following requirements :
c     1st pass : at least one hit per superlayer
c     2nd pass : at least one hit per superlayer with extended searching areas
c     3rd pass : at least 3 hits in the SSD/SVT layers 
c     4th pass : at least 2 hits in the SSD/SVT layers
c     5th pass : at least 1 hit in the SSD or at least a hit 
c                in the SVT layers 2 and 3 

      do pas=1,5
         est_ctrl(1).pass = pas
         write(6,*)'Pass number = ',pas
         do pl=1,est_ctrl(1).maxloop ! loop over pt ranges
            write(6,45)'Loop :',pl,' minimal pt =',
     +           est_ctrl(1).apmin(pl),' GeV/c'
 45         format(a6,i2,a13,f7.4,a6)
            est_ctrl(1).current_loop=pl
            
            do llay=est_ctrl(1).maxlay,est_ctrl(1).minlay,-1 ! loop over layers
               if (est_ctrl(1).ktrl1.eq.1) write(6,*)'Layer #',llay
               est_ctrl(1).layer=llay

c     Looking for association in the current layer
               CALL est_track(geom_h, geom, 
     >              shape_h, shape,
     >              activea_h, activea,
     >              srspar_h, srspar,
     >              config_h, config, 
     >              scs_spt_h,scs_spt,
     >              eval_h, eval,
     >              est_ctrl_h, est_ctrl,
     >              est_match_h, est_match)

c     Refitting of the track with the new associated hit
               CALL est_refit2(tphit_h, tphit,
     >              tptrack_h , tptrack,
     >              est_ctrl_h, est_ctrl,
     >              est_match_h, est_match,
     >              egrpar_h,egrpar)
            enddo               ! layer

            if (est_ctrl(1).ktrl1.eq.1) print *,'pt range finished.'

c     Flag tracks with at least 1 point in the svt
            nremove=0
            nkeep=0
            do i=1,est_match_h.nok
               if (est_match(i).n_svt_hits.gt.0.and.
     +              est_match(i).flag.eq.0) then
                  iret=est_erasor(est_ctrl(1).maxlay,i,pas,
     +                 est_match_h,est_match,tptrack_h,tptrack,
     +                 scs_spt_h,scs_spt)
                  if (iret.eq.1) then
                     nremove=nremove+1
                  else if (iret.eq.0) then
                     nkeep=nkeep+1
                  endif
               endif

c     Flag set to 1 ---> track is Ok
               if (est_match(i).n_svt_hits.gt.0.and.
     +              est_match(i).flag.eq.0) then
                  est_match(i).flag=1
                  est_match(i).binpt=(100*pas)+pl
               endif
            end do
            
c     Refit removed tracks in the 3 last pt bins
            if(pl.eq.est_ctrl(1).maxloop) then
               pl4=1
            else
               pl4=max0(pl-2,1)
            endif
            
            if (est_ctrl(1).ktrl1.eq.1)then
               write(6,*)'nkeep, nremove',nkeep,nremove
            endif

            if (est_ctrl(1).ktrl1.eq.1)then
               write(6,*)'****** PREREFIT SPECIAL LOOP ******'
            endif

c     
            do pll=pl4,pl       
               est_ctrl(1).current_loop=pll
               if (est_ctrl(1).prerefit(pll).ne.-1) then
                  CALL est_prerefit2(tphit_h, tphit,
     >                 tptrack_h, tptrack, 
     >                 est_ctrl_h, est_ctrl,
     >                 est_match_h, est_match,
     >                 egrpar_h,egrpar)
               endif
            enddo
            if (est_ctrl(1).ktrl1.eq.1) then
               write(6,*)'*** END OF PREREFIT SPECIAL LOOP ***'
            endif
222        continue
         end do                 ! loop over pt range
      enddo                     ! pass
      return
      end



C---------------------------------------------------------------------
      INTEGER*4 FUNCTION EST_ERASOR(maxl,itrk,pass,est_match_h,est_match,
     +     tptrack_h,tptrack,scs_spt_h,scs_spt)
 
      IMPLICIT NONE
#include "PAM.inc"
#include "est_match.inc"
#include "tpt_track.inc"
#include "scs_spt.inc"
#include "est_param_def.inc"
C__________________________________________________________________

      RECORD /table_head_st/    est_match_h
      RECORD /est_match_st/    est_match(*)
      RECORD /table_head_st/    tptrack_h
      RECORD /tpt_track_st/    tptrack(*)
      RECORD /table_head_st/    scs_spt_h
      RECORD /scs_spt_st/    scs_spt(*)
C_________________________________________________________________
      INTEGER itrk,pass,j,maxl
      integer waf_min(4),waf_max(4)
      logical layhit, erasor
      
      dimension layhit(4)

      DATA waf_min /1000, 3000, 5000, 7000/
      DATA waf_max /2999, 4999, 6999, 8999/


      if (pass.eq.1.or.pass.eq.2) then
         erasor = .false.
         
         if (est_match(itrk).n_svt_hits.lt.4) then
            erasor = .true.
         elseif (est_match(itrk).n_svt_hits.ge.4) then
            layhit(1)=.false.
            layhit(2)=.false.
            layhit(3)=.false.
            layhit(4)=.false.
            do j=1,est_match(itrk).n_svt_hits
               if (est_match(itrk).id_det(j).ge.waf_min(1) 
     +              .and.est_match(itrk).id_det(j).le.waf_max(1)) 
     +              layhit(1)=.true.
               if (est_match(itrk).id_det(j).ge.waf_min(2) 
     +              .and.est_match(itrk).id_det(j).le.waf_max(2))
     +              layhit(2)=.true.
               if (est_match(itrk).id_det(j).ge.waf_min(3) 
     +              .and.est_match(itrk).id_det(j).le.waf_max(3))
     +              layhit(3)=.true.
               if (est_match(itrk).id_det(j).ge.waf_min(4) 
     +              .and.est_match(itrk).id_det(j).le.waf_max(4)) 
     +              layhit(4)=.true.
            enddo
            if (.not.(layhit(1).and.layhit(2).and.layhit(3).and.
     +           layhit(4))) erasor=.true.
         endif
      endif         


      if (pass.eq.3) then
         erasor = .false.
         
         if (est_match(itrk).n_svt_hits.lt.3) then
            erasor = .true.
         elseif (est_match(itrk).n_svt_hits.ge.3) then
            layhit(1)=.false.
            layhit(2)=.false.
            layhit(3)=.false.
            layhit(4)=.false.
            do j=1,est_match(itrk).n_svt_hits
               if (est_match(itrk).id_det(j).ge.waf_min(1)
     +              .and.est_match(itrk).id_det(j).le.waf_max(1)) 
     +              layhit(1)=.true.
               if (est_match(itrk).id_det(j).ge.waf_min(2) 
     +              .and.est_match(itrk).id_det(j).le.waf_max(2)) 
     +              layhit(2)=.true.
               if (est_match(itrk).id_det(j).ge.waf_min(3) 
     +              .and.est_match(itrk).id_det(j).le.waf_max(3)) 
     +              layhit(3)=.true.
               if (est_match(itrk).id_det(j).ge.waf_min(4) 
     +              .and.est_match(itrk).id_det(j).le.waf_max(4)) 
     +              layhit(4)=.true.
            enddo
            if (.not.((layhit(4).and.((layhit(3).and.layhit(2)).or.
     >           (layhit(3).and.layhit(1)).or.(layhit(2).and.layhit(1))))
     >           .or.(layhit(3).and.layhit(2).and.layhit(1)))) 
     >           erasor=.true.
         endif
      endif         


      if (pass.eq.4) then
         erasor = .false.
         
         if (est_match(itrk).n_svt_hits.lt.2) then
            erasor=.true.
         else
            layhit(1)=.false.
            layhit(2)=.false.
            layhit(3)=.false.
            layhit(4)=.false.
            
            do j=1,est_match(itrk).n_svt_hits
               if (est_match(itrk).id_det(j).ge.waf_min(1) 
     +              .and.est_match(itrk).id_det(j).le.waf_max(1))
     +              layhit(1)=.true.
               if (est_match(itrk).id_det(j).ge.waf_min(2)
     +              .and.est_match(itrk).id_det(j).le.waf_max(2))
     +              layhit(2)=.true.
               if (est_match(itrk).id_det(j).ge.waf_min(3) 
     +              .and.est_match(itrk).id_det(j).le.waf_max(3))
     +              layhit(3)=.true.
               if (est_match(itrk).id_det(j).ge.waf_min(4)
     +              .and.est_match(itrk).id_det(j).le.waf_max(4)) 
     +              layhit(4)=.true.
            enddo
            if (.not.((layhit(4).and.(layhit(3).or.layhit(2).or.
     >           layhit(1))).or.(layhit(3).and.(layhit(2).or.layhit(1)))
     >           .or.(layhit(2).and.layhit(1)))) erasor=.true.
         endif
      endif





      if (pass.eq.5) then
         erasor = .false.
         
         layhit(1)=.false.
         layhit(2)=.false.
         layhit(3)=.false.
         layhit(4)=.false.
         do j=1,est_match(itrk).n_svt_hits
            if (est_match(itrk).id_det(j).ge.waf_min(1)
     +           .and.est_match(itrk).id_det(j).le.waf_max(1)) 
     +           layhit(1)=.true.
            if (est_match(itrk).id_det(j).ge.waf_min(2)
     +           .and.est_match(itrk).id_det(j).le.waf_max(2)) 
     +           layhit(2)=.true.
            if (est_match(itrk).id_det(j).ge.waf_min(3)
     +           .and.est_match(itrk).id_det(j).le.waf_max(3)) 
     +           layhit(3)=.true.
            if (est_match(itrk).id_det(j).ge.waf_min(4)
     +           .and.est_match(itrk).id_det(j).le.waf_max(4))
     +           layhit(4)=.true.
         enddo
         if (maxl.eq.4) then
            if (.not.((layhit(4))
     >           .or.(layhit(3).and.layhit(2)))) erasor=.true.
         else
            if (.not.((layhit(3))
     >           .or.(layhit(2).and.layhit(1)))) erasor=.true.
         endif         
      endif
      
      
      if (erasor) then
         do j=1,NUM_SVT_HIT
            if (est_match(itrk).id_hit(j).ne.0) 
     +           scs_spt(est_match(itrk).id_hit(j)).flag=0
            est_match(itrk).id_hit(j)   =0
            est_match(itrk).z_4th(j)    =0.
            est_match(itrk).phi_4th(j)  =0.
            est_match(itrk).r_4th(j)    =0.
            est_match(itrk).id_det(j)   =0
            est_match(itrk).ang_det(j)  =0.
            est_match(itrk).id_mctrsvt(j)= 0
            est_match(itrk).phi_err(j)  =0.
            est_match(itrk).z_err(j)    =0.
            est_match(itrk).dist(j)     =0.
            est_match(itrk).rflag(j)    =0
         enddo
         est_match(itrk).invp = tptrack(itrk).invp
         est_match(itrk).invp_tpc = tptrack(itrk).invp ! original 
         est_match(itrk).phi0 = tptrack(itrk).phi0
         est_match(itrk).psi = tptrack(itrk).psi
         est_match(itrk).r0 = tptrack(itrk).r0
         est_match(itrk).tanl = tptrack(itrk).tanl
         est_match(itrk).z0 = tptrack(itrk).z0
         est_match(itrk).layer=maxl+1
         est_match(itrk).flag=-1
         est_match(itrk).n_svt_hits=0
      endif
      
      
      if (erasor) then
         est_erasor=1
      else
         est_erasor=0
      endif


      return
      end
