c:>------------------------------------------------------------------
C:FILE:         ems_interface2.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       W.J. Llope, Rice University 
C:BUGS:         None known.
C:HISTORY:      31jul96-v1.0-wjl- Creation.
C:HISTORY:      02may97-v1.1-akio Add Endcap.
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION EMS_INTERFACE2(
     1     g2t_event_h,    g2t_event    
     2    ,g2t_vertex_h,   g2t_vertex   
     3    ,g2t_track_h,    g2t_track    
     4    ,g2t_emc_hit_h,  g2t_emc_hit  ,g2t_smd_hit_h,  g2t_smd_hit 
     5    ,g2t_eem_hit_h,  g2t_eem_hit  ,g2t_esm_hit_h,  g2t_esm_hit
     6    ,ems_control_h,  ems_control
     7    ,ems_hits_bemc_h,ems_hits_bemc,ems_hits_bsmd_h,ems_hits_bsmd
     8    ,ems_hits_eemc_h,ems_hits_eemc,ems_hits_esmd_h,ems_hits_esmd)     
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
C           all hits and hits from incident neutrals...
c:>------------------------------------------------------------------
      IMPLICIT NONE
C
#include "ems_interface2.inc"
C
      INTEGER I,J,K,L,ihit 
      INTEGER emc_ivid(6)/10000000,100000,1000,100,10,1/  !!! matched with AGI and G2T...
      INTEGER eemc_ivid(4)/100000,1000,10,1/              !!! check again-akio
      INTEGER emc_chid(6)                                 !!! matched with AGI and G2T...
      INTEGER smd_ivid(4)/10000000,100000,1000,1/         !!! matched with AGI and G2T...
      INTEGER esmd_ivid(4)/10000000,100000,100,1/         !!! check again -akio
      INTEGER smd_chid(4)                                 !!! matched with AGI and G2T...
      INTEGER emc_id, emc_tr, emc_vid
      INTEGER smd_id, smd_tr, smd_vid
      INTEGER i_eta, i_phi, i_dep, nout, i_seta, i_sphi, i_rl
      INTEGER i_tow1,i_eta1,i_seta2, i_etau, i_phiu, i_rlu
      INTEGER ivid, itrk, ivtx
      REAL    pi
      REAL    emc_x, emc_y, emc_z, emc_Px, emc_Py, emc_Pz
      REAL    emc_de, smd_edep
      INTEGER emc_pid     ! either geant ID or event generator ID...
      INTEGER emc_proc    ! either geant process or event generator process...
      REAL    emc_chg
      INTEGER smd_pid     ! either geant ID or event generator ID...
      REAL    smd_chg
      LOGICAL first_call/.TRUE./

      REAL    parent_vert_rho, r, eta
      INTEGER iparvtx,iptrk,iparpid
      REAL    emc_de_tower(0:4,40,120)
      REAL    emc_de_neut(0:4,40,120)
      REAL    smd_de(300,50,60,2)        ! eta bin, phi bin, phi module, r/l
      LOGICAL sum_down/.FALSE./

c---- variables for speedup (1997may2 akio)
      INTEGER NSWAP
      PARAMETER (NSWAP=100000)
      INTEGER i_etav(NSWAP), i_phiv(NSWAP), i_depv(NSWAP)
      INTEGER i_setav(NSWAP),i_sphiv(NSWAP),i_rlv(NSWAP)
c---- variables for endcap (1997may2 akio)
      REAL    eemc_de_tower(0:4,12,60,2)    ! depth, eta, phi, r/l   
      REAL    eemc_de_neut(0:4,12,60,2)     ! depth, eta, phi, r/l   
      REAL    esmd_de(100,600,60,2)         ! eta bin, phi bin, phi module, r/l
c---- variables for control
      LOGICAL bemc_on/.TRUE./
      LOGICAL bsmd_on/.TRUE./
      LOGICAL eemc_on/.TRUE./
      LOGICAL esmd_on/.TRUE./
      INTEGER control_ivid(4)/1000,100,10,1/  
      INTEGER control_chid(4)                 

c---- start executable statements...
c      write(*,*) 'EMS_INTERFACE2:starting...'
      ems_interface2 = STAFCV_BAD		! default is failure...

c---- first call code...
      if(first_call)then
       first_call	= .FALSE.
      end if

c---- initialize...
      pi = ACOS(-1.)
      sum_down = .FALSE.
      
c----
c---- Endcap EMC section start...  (1997may2 Akio)
c----
      nout     = 0
      if(eemc_on) then
       if(g2t_eem_hit_h.nok.ge.NSWAP) then
         write(*,*) 'EMS INTERFACE: Short of swap space. Increas NSWAP!!!'
         goto 999
       endif
       ems_hits_eemc_h.nok = 0		! not necessary...
       do i = 1,2            ! Loop over r/l 
        do j = 1,60          ! Loop over phi module
         do k = 1,12         ! Loop over eta mudule
          do l = 0,4         ! Loop over depth (0=sum of 1 to 4)
           eemc_de_tower(l,k,j,i) = 0.0
           eemc_de_neut(l,k,j,i) = 0.0
          enddo
         end do
        end do
       end do
c---- start the loop over the EEMC hits --------------------------------
       do ihit = 1, g2t_eem_hit_h.nok

c---- get the g2t information...
         emc_id   = g2t_eem_hit(ihit).id
         emc_tr   = g2t_eem_hit(ihit).track_p
         emc_vid  = g2t_eem_hit(ihit).volume_id
c         emc_x    = g2t_eem_hit(ihit).x(1)
c         emc_y    = g2t_eem_hit(ihit).x(2)
c         emc_z    = g2t_eem_hit(ihit).x(3)
c         emc_Px   = g2t_eem_hit(ihit).p(1)
c         emc_Py   = g2t_eem_hit(ihit).p(2)
c         emc_Pz   = g2t_eem_hit(ihit).p(3)
         emc_de   = g2t_eem_hit(ihit).de
         itrk     = g2t_eem_hit(ihit).track_p
         emc_chg  = g2t_track(itrk).charge
         if(g2t_track(itrk).ge_pid.ne.0)then
          emc_pid  = g2t_track(itrk).ge_pid       ! hit has a geant PID
         elseif(g2t_track(itrk).eg_pid.ne.0)then
          emc_pid  = g2t_track(itrk).eg_pid       ! hit has a generator PID
         end if
         ivid = emc_vid                           ! start decoding volume_id...
         do i = 1,4
          emc_chid(i) = ivid/eemc_ivid(i)
          ivid        = ivid - emc_chid(i)*eemc_ivid(i)
         end do
         if(ivid.ne.0)then
          write(*,*)'EMS_INTERFACE2 -- error decoding Endcap EMC volume_id...',
     1       emc_vid
          goto 999
         end if
c----Need to change again for new geometry/g2t  -akio
         i_rl   = emc_chid(1)        ! right/left: =1 for Z>0, and =2 for Z<0
         i_phi  = emc_chid(2)        ! module phi [1,60]
         i_eta  = emc_chid(3)        ! module eta(radius) [1,12]
         i_dep  = emc_chid(4)        ! depth section [1,4]
            
c----Temporaly eta bin calculated here! -akio
c----to have compativility with old dsl
         if(i_eta.eq.0) then
          r=sqrt(emc_x*emc_x+emc_y*emc_y)
          eta=-1.0*log(tan(atan2(r,emc_z)/2.0))
          i_eta = 12-int((abs(eta)-1.05)/(2.0-1.05))*12.0
          if(i_eta.gt.12) then
            write(*,*) 'Estimated eta bin is too big', i_eta
            i_eta=12
          elseif(i_eta.lt.1) then
            write(*,*) 'Estimated eta bin is too small', i_eta
           i_eta=1
          endif
         endif
c---- follow the vertices until we get to the parent track that entered the stack...
         iparpid = emc_pid
         iparvtx = g2t_track(itrk).start_vertex_p
         k  = 0
 101     Continue
         k  = k + 1
         parent_vert_rho = max( abs(g2t_vertex(iparvtx).ge_x(3)),
     1                          abs(g2t_vertex(iparvtx).ge_x(3)) )
         if(parent_vert_rho.gt.ems_control(1).eemc_inner_z)then
          if(g2t_vertex(iparvtx).n_parent.gt.0)then
           iptrk    = g2t_vertex(iparvtx).parent_p
           iparvtx  = g2t_track(iptrk).start_vertex_p
           if(g2t_track(iptrk).ge_pid.ne.0)then
             iparpid  = g2t_track(iptrk).ge_pid        ! parent has a geant ID...
           elseif(g2t_track(iptrk).eg_pid.ne.0)then
             iparpid  = g2t_track(iptrk).eg_pid        ! parent has a generator ID...
           end if
           if(k.gt.50)then
            write(*,*)
     1       'EMS_INTERFACE2 -- Lots of Parent Vertices!! ',k,ihit,itrk
           end if
           goto 101
          else
           write(*,*)'EMS_INTERFACE2: Lost vertex outside of EEMC' 
          endif
         end if

c---- increment the sums...
c----  To be changed according to new geometry
c----  For the moment, nothing to be done here
c----  Although keep space for new and more complicated geometry
c----                                           -akio
         i_rlu  = i_rl
         i_phiu = i_phi
         i_etau = i_eta
c----- Now take sum
         eemc_de_tower(i_dep,i_etau,i_phiu,i_rlu)
     1            = eemc_de_tower(i_dep,i_etau,i_phiu,i_rlu) 
     1            + emc_de
         eemc_de_tower(    0,i_etau,i_phiu,i_rlu) 
     1            = eemc_de_tower(    0,i_etau,i_phiu,i_rlu) 
     1            + emc_de
         if(iparpid.eq.01.or.     ! increment energy depositions when
     1      iparpid.eq.07.or.     !    incident particle was neutral
     1      iparpid.eq.10.or.     ! (christie neglected photons in TAS Version...)
     1      iparpid.eq.13.or.     !
     1      iparpid.eq.17.or.     !
     1      iparpid.eq.25)then    !
          eemc_de_neut(i_dep,i_etau,i_phiu,i_rlu) 
     1            = eemc_de_neut(i_dep,i_etau,i_phiu,i_rlu) 
     1            + emc_de
          eemc_de_neut(    0,i_etau,i_phiu,i_rlu) 
     1            = eemc_de_neut(    0,i_etau,i_phiu,i_rlu) 
     1            + emc_de
         end if         ! end neutral incident particle check... 
c---Speedup (akio)
         i_rlv(ihit) =i_rlu
         i_etav(ihit)=i_etau
         i_phiv(ihit)=i_phiu
         i_depv(ihit)=i_dep
       end do          ! End loop over input hits
 
c---- fill the ems_hits_eemc output table... Speed Up (akio)
       do i=1, g2t_eem_hit_h.nok
        if(eemc_de_tower(i_depv(i),i_etav(i),i_phiv(i),i_rlv(i)).gt.0.0)then
         if(eemc_de_tower(0 ,i_etav(i),i_phiv(i),i_rlv(i)).gt.0.0)then
          if(nout.lt.ems_hits_eemc_h.maxlen)then
           nout = nout + 1
           ems_hits_eemc(nout).rl            = i_rlv(i)
           ems_hits_eemc(nout).tower_eta_bin = i_etav(i)
           ems_hits_eemc(nout).tower_phi_bin = i_phiv(i)
           ems_hits_eemc(nout).tower_dep     = 0
           ems_hits_eemc(nout).eb_sum  =
     1         eemc_de_tower(0,i_etav(i),i_phiv(i),i_rlv(i))
           ems_hits_eemc(nout).eb_neut =
     1         eemc_de_neut(0,i_etav(i),i_phiv(i),i_rlv(i))
           eemc_de_tower(0,i_etav(i),i_phiv(i),i_rlv(i))=0.0
          else
           write(*,*)'EMS_INTERFACE2 ems_hits_eemc is full'
           write(*,*)'WARNING!!! increase .maxlen and redo...'
          endif
         endif
         if(nout.lt.ems_hits_eemc_h.maxlen)then
          nout = nout + 1
          ems_hits_eemc(nout).rl            = i_rlv(i)
          ems_hits_eemc(nout).tower_eta_bin = i_etav(i)
          ems_hits_eemc(nout).tower_phi_bin = i_phiv(i)
          ems_hits_eemc(nout).tower_dep     = i_depv(i)
          ems_hits_eemc(nout).eb_sum  =
     1      eemc_de_tower(i_depv(i),i_etav(i),i_phiv(i),i_rlv(i))
          ems_hits_eemc(nout).eb_neut =
     1      eemc_de_neut(i_depv(i),i_etav(i),i_phiv(i),i_rlv(i))
          eemc_de_tower(i_depv(i),i_etav(i),i_phiv(i),i_rlv(i))=0.0
         else
          write(*,*)'EMS_INTERFACE2 ems_hits_eemc is full'
          write(*,*)'WARNING!!! increase .maxlen and redo...'
         endif
        endif
       enddo
      endif                     !end of on/off control
      ems_hits_eemc_h.nok = nout
c---- end of EEMC hits section...

c---- start the loop over the ESMD hits -----------------------------------------
      nout     = 0
      if(esmd_on) then
       if(g2t_esm_hit_h.nok.ge.NSWAP) then
         write(*,*) 'EMS INTERFACE: Short of swap space. Increas NSWAP!!!'
         goto 999
       endif
       ems_hits_esmd_h.nok = 0		! not necessary...
       do i_rl = 1,2         ! Loop over r/l
        do i_phi = 1,60      ! Loop over phi module
         do i_sphi = 1,50    ! Loop over phi psuedo pixels (~wires)
          do i_seta = 1,600  ! Loop over eta psuedo pixels (~strips)
           esmd_de(i_seta,i_sphi,i_phi,i_rl) = 0.0
          end do
         end do
        end do
       end do
       do ihit = 1, g2t_esm_hit_h.nok
c---- get the g2t information...
         smd_id   = g2t_esm_hit(ihit).id
         smd_tr   = g2t_esm_hit(ihit).track_p
         smd_vid  = g2t_esm_hit(ihit).volume_id
         smd_edep = g2t_esm_hit(ihit).de
         itrk     = g2t_esm_hit(ihit).track_p
         smd_chg  = g2t_track(itrk).charge
         if(g2t_track(itrk).ge_pid.ne.0)then
          smd_pid  = g2t_track(itrk).ge_pid       ! hit has a geant PID
         elseif(g2t_track(itrk).eg_pid.ne.0)then
          smd_pid  = g2t_track(itrk).eg_pid       ! hit has a generator PID
         end if
         ivid = smd_vid                           ! start decoding volume_id...
         do i = 1,4
          smd_chid(i) = ivid/esmd_ivid(i)
          ivid        = ivid - smd_chid(i)*esmd_ivid(i)
         end do
         if(ivid.ne.0)then
          write(*,*)
     1      'EMS_INTERFACE2 -- error decoding Endcap SMD volume_id...',
     1       smd_vid
          goto 999
         end if
         i_rl   = smd_chid(1)         ! right/left
         i_phi  = smd_chid(2)         ! phi module
         i_sphi = smd_chid(3)         ! smd psuedo wire  (phi information)
         i_seta = smd_chid(4)         ! smd pseudo strip (eta information)

c---- increment the energy array... Need to check again with new geometry -akio
	if((i_rl.lt.1.or.i_rl.gt.2).OR.
     1     (i_phi.lt.1.or.i_phi.gt.60).OR.
     1     (i_sphi.lt.1.or.i_sphi.gt.600).OR.
     1     (i_seta.lt.1.or.i_seta.gt.100))then
         write(*,*)'Wrong module or wire number in Endcap SMD',
     1              i_rl,i_phi,i_sphi,i_seta
        else
         esmd_de(i_seta,i_sphi,i_phi,i_rl)
     1              = esmd_de(i_seta,i_sphi,i_phi,i_rl)
     1              + smd_edep
        end if

c---Speedup (akio)
        i_rlv(ihit)=i_rl
        i_phiv(ihit)=i_phi
        i_sphiv(ihit)=i_sphi
        i_setav(ihit)=i_seta
       end do       ! end loop over SMD hits...

c---- fill the ems_hits_bemc output table... Speed Up (akio)
       do i=1, g2t_esm_hit_h.nok
        if(esmd_de(i_setav(i),i_sphiv(i),i_phiv(i),i_rlv(i)).gt.0.0)then
         if(nout.lt.ems_hits_esmd_h.maxlen)then
          nout = nout + 1
          ems_hits_esmd(nout).rl       = i_rlv(i)
          ems_hits_esmd(nout).phi_mod  = i_phiv(i)
          ems_hits_esmd(nout).phi_strip= i_sphiv(i)
          ems_hits_esmd(nout).eta_strip= i_setav(i)
          ems_hits_esmd(nout).edep    =
     1           esmd_de(i_setav(i),i_sphiv(i),i_phiv(i),i_rlv(i))
          esmd_de(i_setav(i),i_sphiv(i),i_phiv(i),i_rlv(i))=0.0
         else
          write(*,*)'EMS_INTERFACE2 ems_hits_bsmd is full'
          write(*,*)'WARNING!!! increase .maxlen and redo...'
         endif
        endif
       enddo
      endif                     !end of on/off control
      ems_hits_esmd_h.nok = nout
c---- end of ESMD hits section...

c---- end of routine...
      ems_interface2 = STAFCV_OK
999   RETURN
      END

C=======================================================================
