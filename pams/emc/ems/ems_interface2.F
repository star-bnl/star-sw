C:>------------------------------------------------------------------
C:FILE:         ems_interface2.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       OGAWA, Akio
C:BUGS:         None known.
C:HISTORY:      08may98 renewed by akio
c:>------------------------------------------------------------------
#include "emc_def.h"
        
      INTEGER*4 FUNCTION EMS_INTERFACE2(
     1     g2t_event_h,    g2t_event    
     2    ,g2t_vertex_h,   g2t_vertex   
     3    ,g2t_track_h,    g2t_track    
     4    ,g2t_emc_hit_h,  g2t_emc_hit  ,g2t_smd_hit_h,  g2t_smd_hit 
     5    ,g2t_eem_hit_h,  g2t_eem_hit  ,g2t_esm_hit_h,  g2t_esm_hit
     6    ,calb_calg_h,    calb_calg
     7    ,ems_control_h,  ems_control
     8    ,ems_hits_bemc_h,ems_hits_bemc,ems_hits_bsmd_h,ems_hits_bsmd
     9    ,ems_hits_eemc_h,ems_hits_eemc,ems_hits_esmd_h,ems_hits_esmd)
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMS_INTERFACE
C:DESCRIPTION:  produces input tables for ems from g2t tables
C:ARGUMENTS:    g2t_event g2t_vertex g2t_track ems_control
C:ARGUMENTS:    g2t_emc_hit g2t_smd_hit g2t_eem_hit g2t_esm_hit
C:ARGUMENTS:    ems_hits_bemc ems_hits_bsmd ems_hits_eemc ems_hits_esmd
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
C         this is the interface between g2t and ems...
C         includes barrel towers and barrel SMD...
C         it follows the pointers, and increments tower/depth sums for
C         all hits and hits from incident neutrals...
c:>------------------------------------------------------------------
      IMPLICIT NONE
C
#include "ems_interface2.inc"
C
      INTEGER i,j,nout,ihit,ivid,itrk,ipartrk,iparpid,t
      INTEGER module,eta,sub,dep,type,strip,rl,sphi,seta
      REAL    de, shift(2)
c---- Tags
      integer tbemc(120,20,2,2)
      integer tbsmd(120,150,15,2)
      integer teemc(24,12,5,2)
      integer tesmd(24,12,100,2)

c---- start executable statements...
c      write(*,*) 'EMS_INTERFACE2:starting...'
      EMS_INTERFACE2 = STAFCV_BAD

      shift(1)=calb_calg(1).shift(1)
      shift(2)=calb_calg(1).shift(2)

c----
c---- Barrel EMC/PRS
c----
      if(ems_control(1).bemc_on.eq.1 .and. g2t_emc_hit_h.nok.gt.0) then
         call vzero(tbemc, 120*20*2*2)
         nout     = 0
         do ihit = 1, g2t_emc_hit_h.nok
            ivid = g2t_emc_hit(ihit).volume_id            
            itrk = g2t_emc_hit(ihit).track_p
            de = g2t_emc_hit(ihit).de
            call volid_bemc(ivid,module,eta,sub,dep,shift)
            call find_parent_b(ems_control_h, ems_control,
     +           g2t_vertex_h,g2t_vertex,g2t_track_h,g2t_track,itrk,ipartrk)
            iparpid  = g2t_track(itrk).ge_pid
            do j=1,2
               if(j.eq.2) then
                  if(dep.eq.1) then 
                     dep=2      !if it's pre-shower, add dE to the whole tower too
                  else
                     goto 100
                  endif
               endif
               if(tbemc(module,eta,sub,dep).eq.0) then
                  nout=nout+1
                  if(ems_hits_bemc_h.maxlen.le.nout) then
                     write(*,*) 'EMS INTERFACE2: Short of space in EMS_HITS_BEMC'
                     RETURN
                  endif
                  tbemc(module,eta,sub,dep)=nout
                  if(dep.eq.1) then
                     ems_hits_bemc(nout).det=BPRS
                  else
                     ems_hits_bemc(nout).det=BEMC
                  endif
                  ems_hits_bemc(nout).module= module
                  ems_hits_bemc(nout).eta   = eta
                  ems_hits_bemc(nout).sub   = sub
                  ems_hits_bemc(nout).energy = de
                  if(g2t_track(ipartrk).charge.eq.0) then
                     ems_hits_bemc(nout).e_neut=de
                  else
                     ems_hits_bemc(nout).e_neut=0.0
                  endif
               else
                  i=tbemc(module,eta,sub,dep)
                  ems_hits_bemc(i).energy=ems_hits_bemc(i).energy+de
                  if(g2t_track(ipartrk).charge.eq.0) then
                     ems_hits_bemc(i).e_neut=ems_hits_bemc(i).e_neut+de
                  endif
               endif
            enddo
 100        continue
         enddo
         ems_hits_bemc_h.nok=nout
      endif
c----
c---- Barrel SMD
c----
      if(ems_control(1).bsmd_on.eq.1 .and. g2t_smd_hit_h.nok.gt.0) then
         call vzero(tbsmd, 120*150*15*2)
         nout     = 0
         do ihit = 1, g2t_smd_hit_h.nok
            ivid = g2t_smd_hit(ihit).volume_id            
            de = g2t_smd_hit(ihit).de
            call volid_bsmd(ivid,module,eta,sub,type,shift)
            if(type.eq.BSMDE) then
               t=1
            else
               t=2
            endif
            if(tbsmd(module,eta,sub,t).eq.0) then
               nout=nout+1
               if(ems_hits_bsmd_h.maxlen.le.nout) then
                  write(*,*) 'EMS INTERFACE2: Short of space in EMS_HITS_BSMD'
                  RETURN
               endif
               tbsmd(module,eta,sub,t)=nout
               ems_hits_bsmd(nout).det    = type
               ems_hits_bsmd(nout).module = module
               ems_hits_bsmd(nout).eta    = eta
               ems_hits_bsmd(nout).sub    = sub
               ems_hits_bsmd(nout).energy = de
            else
               i=tbsmd(module,eta,sub,t)
               ems_hits_bsmd(i).energy=ems_hits_bsmd(i).energy+de
            endif
         enddo
         ems_hits_bsmd_h.nok=nout
      endif
c----
c---- Endcap EMC/PRS
c----
      if(ems_control(1).eemc_on.eq.1 .and. g2t_eem_hit_h.nok.gt.0) then
         call vzero(teemc, 24*12*5*2)
         nout     = 0
         do ihit = 1, g2t_eem_hit_h.nok
            ivid = g2t_eem_hit(ihit).volume_id            
            itrk = g2t_eem_hit(ihit).track_p
            de = g2t_eem_hit(ihit).de
            call volid_eemc(ivid,module,eta,sub,dep)
            call find_parent_e(ems_control_h, ems_control,
     +           g2t_vertex_h,g2t_vertex,g2t_track_h,g2t_track,itrk,ipartrk)
            iparpid  = g2t_track(itrk).ge_pid
            do j=1,2
               if(j.eq.2) then
                  if(dep.eq.1) then
                     dep=2      !if it's pre-shower, add dE to the whole tower too
                  else
                     goto 110
                  endif
               endif
               if(teemc(module,eta,sub,dep).eq.0) then
                  nout=nout+1
                  if(ems_hits_eemc_h.maxlen.le.nout) then
                     write(*,*) 'EMS INTERFACE2: Short of space in EMS_HITS_EEMC'
                     RETURN
                  endif
                  teemc(module,eta,sub,dep)=nout
                  if(dep.eq.1) then
                     ems_hits_eemc(nout).det=EPRS
                  else
                     ems_hits_eemc(nout).det=EEMC
                  endif
                  ems_hits_eemc(nout).module = module
                  ems_hits_eemc(nout).eta    = eta
                  ems_hits_eemc(nout).sub    = sub
                  ems_hits_eemc(nout).energy = de
                  if(g2t_track(ipartrk).charge.eq.0) then
                     ems_hits_eemc(nout).e_neut=de
                  else
                     ems_hits_eemc(nout).e_neut=0.0
                  endif
               else
                  i=teemc(module,eta,sub,dep)
                  ems_hits_eemc(i).energy=ems_hits_eemc(i).energy+de
                  if(g2t_track(ipartrk).charge.eq.0) then
                     ems_hits_eemc(i).e_neut=ems_hits_eemc(i).e_neut+de
                  endif
               endif
            enddo
 110        continue
         enddo
         ems_hits_eemc_h.nok=nout
      endif
c----
c---- Endcap SMD
c----
      if(ems_control(1).esmd_on.eq.1 .and. g2t_esm_hit_h.nok.gt.0) then
         nout = 0
         call vzero(tesmd, 24*12*100*2)
         do ihit = 1, g2t_esm_hit_h.nok
            ivid = g2t_esm_hit(ihit).volume_id            
            de = g2t_esm_hit(ihit).de
            call volid_esmd(ivid,module,eta,sub,type)
            if(tesmd(module,eta,sub,type).eq.0) then
               nout=nout+1
               if(ems_hits_esmd_h.maxlen.le.nout) then
                  write(*,*) 'EMS INTERFACE2: Short of space in EMS_HITS_ESMD'
                  return
               endif
               tesmd(module,eta,sub,type) =nout
               if(type.eq.1) then
                  ems_hits_esmd(nout).det = ESMDE
               else
                  ems_hits_esmd(nout).det = ESMDP
               endif
               ems_hits_esmd(nout).module    = module
               ems_hits_esmd(nout).eta       = eta
               ems_hits_esmd(nout).sub       = sub
               ems_hits_esmd(nout).energy    = de
            else
               i=tesmd(module,eta,sub,type)
               ems_hits_esmd(i).energy=ems_hits_esmd(i).energy+de
            endif
         enddo
         ems_hits_esmd_h.nok=nout
      endif
      write(*,*) 
     +     'EMS_INTERFACE2:finished with # of hits at B/E EMC/SMD:',
     +     ems_hits_bemc_h.nok,ems_hits_bsmd_h.nok,
     +     ems_hits_eemc_h.nok,ems_hits_esmd_h.nok
      EMS_INTERFACE2 = STAFCV_OK
      return
      end
