c:>------------------------------------------------------------------
C:FILE:         ems_interface.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       W.J. Llope, Rice University 
C:BUGS:         None known.
C:HISTORY:      31jul96-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION EMS_INTERFACE(
     1         g2t_event_h,       g2t_event ,
     2         g2t_vertex_h,      g2t_vertex ,
     3         g2t_track_h,       g2t_track ,
     4         g2t_emc_hit_h,     g2t_emc_hit ,
     5         g2t_smd_hit_h,     g2t_smd_hit ,
     6         ems_control_h,     ems_control ,
     6         ems_hits_bemc_h,   ems_hits_bemc ,
     7         ems_hits_bsmd_h,   ems_hits_bsmd )
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMS_INTERFACE
C:DESCRIPTION:  produces input tables for ems from g2t tables
C:ARGUMENTS:    g2t_event g2t_vertex g2t_track g2t_emc_hit g2t_smd_hit ems_control
C:ARGUMENTS:    ems_hits_bemc ems_hits_bsmd
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
C         this is the interface between g2t and ems...
C         includes barrel towers and barrel SMD...
C         it follows the pointers, and increments tower/depth sums for
C           all hits and hits from incident neutrals...
c:>------------------------------------------------------------------
      IMPLICIT NONE
C
#include "ems_interface.inc"
C
      INTEGER I,J,K,ihit 
      INTEGER emc_ivid(6)/10000000,100000,1000,100,10,1/     !!! matched with AGI and G2T...
      INTEGER emc_chid(6)                                    !!! matched with AGI and G2T...
      INTEGER smd_ivid(4)/10000000,100000,1000,1/            !!! matched with AGI and G2T...
      INTEGER smd_chid(4)                                    !!! matched with AGI and G2T...
      INTEGER emc_id, emc_tr, emc_vid
      INTEGER smd_id, smd_tr, smd_vid
      INTEGER i_eta, i_phi, i_dep, nout, i_seta, i_sphi, i_rl
      INTEGER i_tow1,i_eta1,i_etau,i_phiu,i_seta2,nphi,neta
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

      REAL    parent_vert_rho
      INTEGER iparvtx,iptrk,iparpid
      INTEGER inc_parent_pid(100000)      ! index is hit number
      INTEGER inc_parent_trk(100000)      ! index is hit number
      REAL    emc_de_tower(0:4,40,120)
      REAL    emc_de_neut(0:4,40,120)
      REAL    smd_de(300,50,60,2)        ! eta bin, phi bin, phi module, r/l
      LOGICAL sum_down/.FALSE./

c---- start executable statements...
      ems_interface = STAFCV_BAD		! default is failure...

c---- first call code...
      if(first_call)then
       first_call	= .FALSE.
      end if

c---- initialize...
      pi = ACOS(-1.)
      ems_hits_bemc_h.nok = 0		! not necessary...
      ems_hits_bsmd_h.nok = 0		! not necessary...
      do i = 1,120
       do j = 1,40
        do k = 0,4
         emc_de_tower(k,j,i) = 0.0
         emc_de_neut(k,j,i) = 0.0
        end do
       end do
      end do
      do i_rl = 1,2         ! Loop over r/l
       do i_phi = 1,60      ! Loop over phi module
        do i_sphi = 1,50    ! Loop over phi psuedo pixels (~wires)
         do i_seta = 1,300  ! Loop over eta psuedo pixels (~strips)
           smd_de(i_seta,i_sphi,i_phi,i_rl) = 0.0
         end do
        end do
       end do
      end do
      sum_down = .FALSE.
      if(ems_control(1).bemc_Ntowers.eq.1200)
     1   sum_down = .TRUE.

c---- start the loop over the BEMC hits -----------------------------------------
      do ihit = 1, g2t_emc_hit_h.nok

c---- get the g2t information...
         emc_id   = g2t_emc_hit(ihit).id
         emc_tr   = g2t_emc_hit(ihit).track_p
         emc_vid  = g2t_emc_hit(ihit).volume_id
         emc_x    = g2t_emc_hit(ihit).x(1)
         emc_y    = g2t_emc_hit(ihit).x(2)
         emc_z    = g2t_emc_hit(ihit).x(3)
         emc_Px   = g2t_emc_hit(ihit).p(1)
         emc_Py   = g2t_emc_hit(ihit).p(2)
         emc_Pz   = g2t_emc_hit(ihit).p(3)
         emc_de   = g2t_emc_hit(ihit).de
         itrk     = g2t_emc_hit(ihit).track_p
         emc_chg  = g2t_track(itrk).charge
         if(g2t_track(itrk).ge_pid.ne.0)then
          emc_pid  = g2t_track(itrk).ge_pid       ! hit has a geant PID
         elseif(g2t_track(itrk).eg_pid.ne.0)then
          emc_pid  = g2t_track(itrk).eg_pid       ! hit has a generator PID
         end if
         ivid = emc_vid                           ! start decoding volume_id...
         do i = 1,6
          emc_chid(i) = ivid/emc_ivid(i)
          ivid        = ivid - emc_chid(i)*emc_ivid(i)
         end do
         if(ivid.ne.0)then
          write(*,*)'EMS_INTERFACE -- error decoding EMC volume_id...',
     1       emc_vid
          goto 999
         end if
         i_rl   = emc_chid(1)             ! right/left: =1 for Z>0, and =2 for Z<0
         i_phi  = emc_chid(2)             ! module phi [1,60]
         i_tow1 = emc_chid(3)             ! d(eta)=0.1 tower number [1,10]
         i_dep  = emc_chid(4)             ! depth section [1,4]
         i_sphi = emc_chid(5)             ! sub-phi [1,2] 
         i_seta = emc_chid(6)             ! sub-eta [1,2] 

c---- follow the vertices until we get to the parent track that entered the stack...
         inc_parent_pid(ihit) = emc_pid
         iparvtx              = g2t_track(itrk).start_vertex_p 
         k  = 0
100      Continue
         k  = k + 1
         parent_vert_rho   = MAX(
     1                       SQRT(g2t_vertex(iparvtx).ge_x(1)**2     ! parent has geant ID...
     1                     +      g2t_vertex(iparvtx).ge_x(2)**2),   !
     1                       SQRT(g2t_vertex(iparvtx).eg_x(1)**2     ! parent has generator ID...
     1                     +      g2t_vertex(iparvtx).eg_x(2)**2))   !
         if(parent_vert_rho.lt.ems_control(1).bemc_inner_r)then
          inc_parent_pid(ihit) = iparpid
          inc_parent_trk(ihit) = iptrk
         else
          if(g2t_vertex(ivtx).n_parent.gt.0)then
           iptrk    = g2t_vertex(iparvtx).parent_p
           iparvtx  = g2t_track(iptrk).start_vertex_p 
           if(g2t_track(iptrk).ge_pid.ne.0)then
            iparpid  = g2t_track(iptrk).ge_pid        ! parent has a geant ID...
           elseif(g2t_track(iptrk).eg_pid.ne.0)then
            iparpid  = g2t_track(iptrk).eg_pid        ! parent has a generator ID...
           end if
           if(k.gt.50)then
            write(*,*)
     1       'EMS_INTERFACE -- Lots of Parent Vertices!! ',k,ihit,itrk
           end if
           goto 100
          end if
         end if

c---- increment the sums...
         i_seta2  = 0                      !
         if(i_rl.eq.2)then                 ! negative Z, tower 1 at eta~-1
          i_eta1  = 11 - i_tow1            !    [ 1,10]
          i_seta2 = 3 - i_seta             ! flip it...
          i_etau  = (i_eta1-1)*2 + i_seta2 !    [ 1,20]
         elseif(i_rl.eq.1)then             ! positive Z, tower 11/21 and eta~0
          i_eta1  = 10 + i_tow1            !    [11,20]
          i_etau  = (i_eta1-1)*2 + i_seta  !    [21,40]
         end if                            !
         i_phiu  = (i_phi-1)*2 + i_sphi    !    [1,120]
         if(sum_down)then                  !
          i_etau = i_eta1                  !    [1,20]
          i_phiu = i_phi                   !    [1,60]
         end if                            !
         emc_de_tower(i_dep,i_etau,i_phiu)
     1            = emc_de_tower(i_dep,i_etau,i_phiu) 
     1            + emc_de
         emc_de_tower(    0,i_etau,i_phiu) 
     1            = emc_de_tower(    0,i_etau,i_phiu) 
     1            + emc_de
         if(inc_parent_pid(ihit).eq.01.or.     ! increment energy depositions when
     1      inc_parent_pid(ihit).eq.07.or.     !    incident particle was neutral
     1      inc_parent_pid(ihit).eq.10.or.     ! (christie neglected photons in TAS Version...)
     1      inc_parent_pid(ihit).eq.13.or.     !
     1      inc_parent_pid(ihit).eq.17.or.     !
     1      inc_parent_pid(ihit).eq.25)then    !
          emc_de_neut(i_dep,i_etau,i_phiu) 
     1            = emc_de_neut(i_dep,i_etau,i_phiu) 
     1            + emc_de
          emc_de_neut(    0,i_etau,i_phiu) 
     1            = emc_de_neut(    0,i_etau,i_phiu) 
     1            + emc_de
         end if         ! end neutral incident particle check...
      end do          ! End loop over input hits

c---- fill the ems_hits_bemc output table...       
      nout     = 0
      if(sum_down)then
       nphi =  60
       neta =  20
      else
       nphi = 120
       neta =  40
      end if
      do i_phiu = 1,nphi     ! Loop over phi
       do i_etau = 1,neta    ! Loop over eta
        do i_dep = 0,4       ! Loop over depths (0=tower sum)
         if(nout.lt.ems_hits_bemc_h.maxlen)then
          if(emc_de_tower(i_dep,i_etau,i_phiu).gt.0.0)then
           nout = nout + 1
           ems_hits_bemc(nout).tower_eta_bin = i_etau
           ems_hits_bemc(nout).tower_phi_bin = i_phiu
           ems_hits_bemc(nout).tower_dep     = i_dep
           ems_hits_bemc(nout).eb_sum  = 
     1       emc_de_tower(i_dep,i_etau,i_phiu) 
           ems_hits_bemc(nout).eb_neut = 
     1       emc_de_neut(i_dep,i_etau,i_phiu) 
          end if   ! end energy > 0 check...
         else
          write(*,*)
     1'EMS_INTERFACE -- WARNING!!! ems_hits_bemc is full, rows missed!'
          write(*,*)
     1'              -- WARNING!!! increase .maxlen and redo...'
         end if    ! end maxlen check..
        end do     ! end depth loop...
       end do      ! end eta loop...
      end do       ! end phi loop...
      ems_hits_bemc_h.nok = nout
c---- end of BEMC hits section...

c---- start the loop over the BSMD hits -----------------------------------------
      do ihit = 1, g2t_smd_hit_h.nok

c---- get the g2t information...
         smd_id   = g2t_smd_hit(ihit).id
         smd_tr   = g2t_smd_hit(ihit).track_p
         smd_vid  = g2t_smd_hit(ihit).volume_id
         smd_edep = g2t_smd_hit(ihit).de
         itrk     = g2t_smd_hit(ihit).track_p
         smd_chg  = g2t_track(itrk).charge
         if(g2t_track(itrk).ge_pid.ne.0)then
          smd_pid  = g2t_track(itrk).ge_pid       ! hit has a geant PID
         elseif(g2t_track(itrk).eg_pid.ne.0)then
          smd_pid  = g2t_track(itrk).eg_pid       ! hit has a generator PID
         end if
         ivid = smd_vid                           ! start decoding volume_id...
         do i = 1,4
          smd_chid(i) = ivid/smd_ivid(i)
          ivid        = ivid - smd_chid(i)*smd_ivid(i)
         end do
         if(ivid.ne.0)then
          write(*,*)
     1      'EMS_INTERFACE -- error decoding SMD volume_id...',
     1       smd_vid
          goto 999
         end if
         i_rl   = smd_chid(1)         ! right/left
         i_phi  = smd_chid(2)         ! phi module
         i_sphi = smd_chid(3)         ! smd psuedo wire  (phi information)
         i_seta = smd_chid(4)         ! smd pseudo strip (eta information)

c---- increment the energy array...
		if((i_rl.lt.1.or.i_rl.gt.2).OR.
     1     (i_phi.lt.1.or.i_phi.gt.60).OR.
     1     (i_sphi.lt.1.or.i_sphi.gt.50).OR.
     1     (i_seta.lt.1.or.i_seta.gt.300))then
         write(*,*)i_rl,i_phi,i_dep,i_sphi,i_seta
        else
         smd_de(i_seta,i_sphi,i_phi,i_rl)
     1              = smd_de(i_seta,i_sphi,i_phi,i_rl) 
     1              + smd_edep
        end if

      end do       ! end loop over SMD hits...

c---- fill the ems_hits_bsmd output table...       
      nout     = 0
      do i_rl = 1,2         ! Loop over r/l
       do i_phi = 1,60      ! Loop over phi module
        do i_sphi = 1,50    ! Loop over phi psuedo pixels (~wires)
         do i_seta = 1,300  ! Loop over eta psuedo pixels (~strips)
          if(nout.lt.ems_hits_bsmd_h.maxlen)then
           if(smd_de(i_seta,i_sphi,i_phi,i_rl).gt.0.0)then
            nout = nout + 1
            ems_hits_bsmd(nout).rl      = i_rl
            ems_hits_bsmd(nout).phi_bin = i_phi
            ems_hits_bsmd(nout).pswire  = i_sphi    
            ems_hits_bsmd(nout).psstrip = i_seta
            ems_hits_bsmd(nout).edep    = 
     1                     smd_de(i_seta,i_sphi,i_phi,i_rl) 
           end if           ! end edep > 0 check...
          else
           write(*,*)
     1'EMS_INTERFACE -- WARNING!!! ems_hits_bsmd is full, rows missed!'
           write(*,*)
     1'              -- WARNING!!! increase .maxlen and redo...'
          end if   ! end maxlen check..
         end do    ! end eta pixel loop...
        end do     ! end phi pixel loop...
       end do      ! end phi module loop...
      end do       ! end r/l loop...
      ems_hits_bsmd_h.nok = nout
c---- end of SMD hits section...

c---- end of routine...
      ems_interface = STAFCV_OK
999   RETURN
      END

C=======================================================================
