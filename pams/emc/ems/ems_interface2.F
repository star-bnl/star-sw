C:>------------------------------------------------------------------
C:FILE:         ems_interface2.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       OGAWA, Akio
C:BUGS:         None known.
C:HISTORY:      08may98 renewed by akio
c:>------------------------------------------------------------------
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
      INTEGER i,j,nout,ihit,ivid,itrk,ipartrk,iparpid
      INTEGER eta,phi,dep,type,strip,rl,sphi,seta
      REAL    de

c---- Tags
      integer tbemc(-20:20,240,2)
      integer tbsmd(-10:10,120,3,75)
      integer teemc(-12:12,120,0:3)
      integer tesmd(2,60,200,100)

c---- start executable statements...
c      write(*,*) 'EMS_INTERFACE2:starting...'
      EMS_INTERFACE2 = STAFCV_BAD

c----
c---- Barrel EMC/PRS
c----
      if(ems_control(1).bemc_on.eq.1) then
         call vzero(tbemc, 41*240*2)
         nout     = 0
         do ihit = 1, g2t_emc_hit_h.nok
            ivid = g2t_emc_hit(ihit).volume_id            
            itrk = g2t_emc_hit(ihit).track_p
            de = g2t_emc_hit(ihit).de
            call volid_bemc(ivid,eta,phi,dep,calb_calg(1).shift)
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
               if(tbemc(eta,phi,dep).eq.0) then
                  nout=nout+1
                  if(ems_hits_bemc_h.maxlen.le.nout) then
                     write(*,*) 'EMS INTERFACE2: Short of space in EMS_HITS_BEMC'
                     RETURN
                  endif
                  i=nout
                  tbemc(eta,phi,dep)=nout
                  ems_hits_bemc(i).eta_bin  = eta
                  ems_hits_bemc(i).phi_bin  = phi
                  ems_hits_bemc(i).tower_dep= dep
                  ems_hits_bemc(i).e_sum    = de
                  if(g2t_track(ipartrk).charge.eq.0) then
                     ems_hits_bemc(i).e_neut=de
                  else
                     ems_hits_bemc(i).e_neut=0.0
                  endif
               else
                  i=tbemc(eta,phi,dep)
                  ems_hits_bemc(i).e_sum=ems_hits_bemc(i).e_sum+de
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
      if(ems_control(1).bsmd_on.eq.1) then
         call vzero(tbsmd, 21*120*3*75)
         nout     = 0
         do ihit = 1, g2t_smd_hit_h.nok
            ivid = g2t_smd_hit(ihit).volume_id            
            de = g2t_smd_hit(ihit).de
            call volid_bsmd(ivid,eta,phi,type,strip,calb_calg(1).shift)
            if(tbsmd(eta,phi,type,strip).eq.0) then
               nout=nout+1
               if(ems_hits_bsmd_h.maxlen.le.nout) then
                  write(*,*) 'EMS INTERFACE2: Short of space in EMS_HITS_BSMD'
                  RETURN
               endif
               i=nout
               tesmd(eta,phi,type,strip)=nout
               ems_hits_bsmd(i).eta_bin = eta
               ems_hits_bsmd(i).phi_bin = phi
               ems_hits_bsmd(i).type    = type
               ems_hits_bsmd(i).strip   = strip
               ems_hits_bsmd(i).e_sum   = de
            else
               i=tbsmd(eta,phi,type,strip)
               ems_hits_bsmd(i).e_sum=ems_hits_bsmd(i).e_sum+de
            endif
         enddo
         ems_hits_bsmd_h.nok=nout
      endif
c----
c---- Endcap EMC/PRS
c----
      if(ems_control(1).eemc_on.eq.1) then
         call vzero(teemc, 25*120*4)
         nout     = 0
         do ihit = 1, g2t_eem_hit_h.nok
            ivid = g2t_eem_hit(ihit).volume_id            
            itrk = g2t_eem_hit(ihit).track_p
            de = g2t_eem_hit(ihit).de
            call volid_eemc(ivid,eta,phi,dep)
            call find_parent_e(ems_control_h, ems_control,
     +           g2t_vertex_h,g2t_vertex,g2t_track_h,g2t_track,itrk,ipartrk)
            iparpid  = g2t_track(itrk).ge_pid
            do j=1,2
               if(j.eq.2) dep=0      !depth=0 is the whole tower
               if(teemc(eta,phi,dep).eq.0) then
                  nout=nout+1
                  if(ems_hits_eemc_h.maxlen.le.nout) then
                     write(*,*) 'EMS INTERFACE2: Short of space in EMS_HITS_EEMC'
                     RETURN
                  endif
                  i=nout
                  teemc(eta,phi,dep)=nout
                  ems_hits_eemc(i).eta_bin  = eta
                  ems_hits_eemc(i).phi_bin  = phi
                  ems_hits_eemc(i).tower_dep= dep
                  ems_hits_eemc(i).e_sum    = de
                  if(g2t_track(ipartrk).charge.eq.0) then
                     ems_hits_eemc(i).e_neut=de
                  else
                     ems_hits_eemc(i).e_neut=0.0
                  endif
               else
                  i=teemc(eta,phi,dep)
                  ems_hits_eemc(i).e_sum=ems_hits_eemc(i).e_sum+de
                  if(g2t_track(ipartrk).charge.eq.0) then
                     ems_hits_eemc(i).e_neut=ems_hits_eemc(i).e_neut+de
                  endif
               endif
            enddo
         enddo
         ems_hits_eemc_h.nok=nout
      endif
c----
c---- Endcap SMD
c----
      if(ems_control(1).esmd_on.eq.1) then
         nout     = 0
         call vzero(tesmd, 2*60*200*100)
         do ihit = 1, g2t_esm_hit_h.nok
            ivid = g2t_esm_hit(ihit).volume_id            
            de = g2t_esm_hit(ihit).de
            call volid_esmd(ivid,rl,phi,sphi,seta)
            if(tesmd(rl,phi,sphi,seta).eq.0) then
               nout=nout+1
               if(ems_hits_esmd_h.maxlen.le.nout) then
                  write(*,*) 'EMS INTERFACE2: Short of space in EMS_HITS_ESMD'
                  return
               endif
               i=nout
               tesmd(rl,phi,sphi,seta)=nout
               ems_hits_esmd(i).rl        = rl
               ems_hits_esmd(i).phi_mod   = phi
               ems_hits_esmd(i).phi_strip = sphi
               ems_hits_esmd(i).eta_strip = seta
               ems_hits_esmd(i).e_sum     = de
            else
               i=tesmd(rl,phi,sphi,seta)
               ems_hits_esmd(i).e_sum=ems_hits_esmd(i).e_sum+de
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

