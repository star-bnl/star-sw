      INTEGER*4 FUNCTION EMS_EGRID(
     1     ems_hits_bemc_h,     ems_hits_bemc ,
     2       egr_globtrk_h,       egr_globtrk ,
     3       ems_control_h,       ems_control ,
     4        ems_emecor_h,        ems_emecor ) 
c:>------------------------------------------------------------------
C:FILE:         ems_egrid.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       W.J. Llope, Rice University (TAS Version by B. Christie)
C:BUGS:         None known.
C:HISTORY:      31jul96-v1.0-wjl- Creation.
C:              30oct96 T. LeCompte added some bounds checking
C:              15feb97 WJL, fixed 72 column overflows in lecompte's bounds checks
c:>------------------------------------------------------------------
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMS_EGRID
C:DESCRIPTION:  charged and neutral energy in (eta,phi) grids...
C     Takes in the Table that contains the Total signal in the 
C     EMC barrel towers, the table of the primary vertex info., and the
C     Global track info. Projects the global tracks to the EMC Barrel,
C     calculates the most probable energy deposited in the tower/towers
C     by the track, subtracts the energy due to charged track from the
C     energy in the tower, and puts out resulting tower signals (due to
C     neutrals) in emc_neut table. This module then projects the tracks
C     back to the primary vertex, claculates the Energy, eta, and phi
C     for the tracks and loads a table with the charged energy at the
C     vertex.
C:ARGUMENTS:    ems_hits_bemc egr_globtrk ems_control
C:ARGUMENTS:    ems_emecor
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE
C
#include "ems_egrid.inc"
C
      INTEGER ihit, test, nok
      INTEGER ierr
      INTEGER i,j,k
      REAL trk_qual,trk_qual_cut,trkchar,rval,zval
      REAL pt,pz,p_tot,x_strt,y_strt,cap_phi0
      REAL b_field,deg_to_rad,pi
      REAL xc,yc,ener_subt,crad
      REAL p_input(6)     ! Track pars passed to intercept routine
      REAL ang_int,td_ang,cosphi_int,z_int,dirvec(3),eta_hit
      REAL dpsi_ds,e_vtx,cal
      REAL trak(6),xc1(2),r1,xp(2),xn1(2),trk1(6)
      REAL e_char(120,40)  ! Energy at vtx due to charged globtrks
      REAL en_mct(120,40)  ! Neutral Energy via MCT (i.e. GEANT)
      REAL en_cpr(120,40)  ! Neutral Energy via subtr. of CPR
      REAL etot2(120,40)   ! Total Energy with Echar via gtrk, En via MCT
      REAL etot3(120,40)   ! Total Energy at vtx via MKINE
      REAL etots(120,40)   ! sum of e_char + en_cpr
      REAL numc(120,40)    ! Number of charged tracks in this tower
      INTEGER emc_numbv,emc_itrk,emc_char,emc_pid,emc_pidold,emc_recdig
      INTEGER eta_pm,emc_layer,phi_bin,eta_bin,out_num,num_in
      INTEGER trk_in,itwr, pbin_in, ebin_in, pbin, ebin
      INTEGER int_yn,phi_trk,eta_trk
      INTEGER iascii_out
      INTEGER numc_total
      LOGICAL sum_down/.FALSE./

c---- start executable statements...
      ems_egrid = STAFCV_OK
      trk_qual_cut = ems_control(1).trk_qual_cut
      b_field      = ems_control(1).b_field
      cal          = ems_control(1).sampfrac
      pi           = ACOS(-1.0)
      deg_to_rad   = pi/180.0

C---- Initialize nok counter and variable arrays
      ems_emecor_h.nok = 0
      out_num = 0
      num_in = 0
      do i=1,120    ! Loop over phi_bin number
         do j=1,40 ! Loop over eta_bin number
             e_char(i,j)   = 0.
             en_mct(i,j)   = 0.
             en_cpr(i,j)   = 0.
             etot2(i,j)    = 0.
             etot3(i,j)    = 0.
             etots(i,j)    = 0.
             numc(i,j)     = 0.
         enddo     ! End loop over eta_bin
      enddo        ! end loop over phi_bin
      sum_down = .FALSE.
      if(ems_control(1).bemc_Ntowers.eq.1200)
     1   sum_down = .TRUE.

*     Load Neutral Energy and total Energy
      do itwr = 1,ems_hits_bemc_h.nok
        ebin_in = ems_hits_bemc(itwr).eta_bin
        pbin_in = ems_hits_bemc(itwr).phi_bin
        if(ems_hits_bemc(itwr).tower_dep.eq.0)then     ! tower sum in depth=0...
         en_mct(pbin_in,ebin_in) = ems_hits_bemc(itwr).e_neut
         en_cpr(pbin_in,ebin_in) = ems_hits_bemc(itwr).e_sum
        end if
      enddo

C      get the tracks from GLOBTRK table
cWJL      write(*,*)'EMC_EGRID -- No. tracks in =',egr_globtrk_h.nok
	  numc_total = 0

      trk_in = egr_globtrk_h.nok
      do ihit = 1, trk_in   ! Loop over input tracks
       trk_qual=egr_globtrk(ihit).chisq/float(egr_globtrk(ihit).ndegf)
        if(trk_qual.le.trk_qual_cut)then
          trkchar = float(egr_globtrk(ihit).icharge)
          pt      = 1.0/egr_globtrk(ihit).invpt
          pz      = pt*egr_globtrk(ihit).tanl
          p_tot   = sqrt(pt**2 + pz**2)
          crad    = pt/abs(trkchar*0.0003*b_field)
          x_strt  = egr_globtrk(ihit).r0
     1            * cos(egr_globtrk(ihit).phi0*deg_to_rad)
          y_strt  = egr_globtrk(ihit).r0
     1            * sin(egr_globtrk(ihit).phi0*deg_to_rad)
cWJL      dpsi_ds = trkchar*egr_globtrk(ihit).tanl/abs(egr_globtrk(ihit).tanl)
          cap_phi0= egr_globtrk(ihit).psi*deg_to_rad + trkchar*0.5*pi
          xc      = x_strt - crad*cos(cap_phi0)
          yc      = y_strt - crad*sin(cap_phi0)
          p_input(1) = xc
          p_input(2) = yc
          p_input(3) = egr_globtrk(ihit).z0
          p_input(4) = crad
          p_input(5) = egr_globtrk(ihit).tanl 
          p_input(6) = cap_phi0
          z_int = 10000.
          rval  = ems_control(1).bemc_inner_r
          zval  = ems_control(1).bemc_max_z
          call eut_extra_rad(p_input,rval,trkchar,ang_int,td_ang,
     +                      cosphi_int,z_int,int_yn,dirvec)
          if(int_yn.eq.0.and.abs(z_int).le.zval)then        ! Trk hits EMC Barrel?
            eta_hit = -1.0*log(tan(atan2(rval,z_int)/2.0))
            if(eta_hit.gt.0)then                            ! Positive eta's in bins 21 -> 40.
               eta_bin  = INT(20.0*eta_hit) + 21
               if(eta_bin.gt.40)eta_bin = 40
            else                                            ! Negative eta's in bins 01 -> 20
               eta_bin  = 20 - INT(20.0*abs(eta_hit))
            endif     
            if(ang_int.gt.0)then                            ! Phi angle from 0 -> 180 degrees
                phi_bin = INT(ang_int/(0.10472/2.0)) + 1
            else
                phi_bin = INT(ang_int/(0.10472/2.0)) + 120
            endif
            if(sum_down)then
             eta_bin = ((eta_bin-1)/2) + 1
             phi_bin = ((phi_bin-1)/2) + 1
            end if 
            if(eta_bin.le.40.and.eta_bin.gt.0.and.
     1         phi_bin.le.120.and.phi_bin.gt.0) then
                 numc(phi_bin,eta_bin) = numc(phi_bin,eta_bin) + 1
            end if  ! bound check 
            numc_total = numc_total + 1

*           -----------------------------------------
*           Subtract Energy dep. by track out of Tower
*           Assume all charged tracks are pions
*           -----------------------------------------
            if(p_tot.le.0.35)then
                ener_subt = 0.049*p_tot
            elseif(p_tot.le.0.45)then
                ener_subt = 0.0435*p_tot
            elseif(p_tot.le.0.55)then
                ener_subt = 0.038*p_tot
            elseif(p_tot.le.0.65)then
                ener_subt = 0.032*p_tot
            elseif(p_tot.le.0.75)then
                ener_subt = 0.026*p_tot
            elseif(p_tot.le.0.85)then
                ener_subt = 0.025*p_tot
            elseif(p_tot.le.1.00)then
                ener_subt = 0.024*p_tot
            elseif(p_tot.le.1.30)then
                ener_subt = 0.022*p_tot
            elseif(p_tot.le.2.30)then
                ener_subt = 0.019*p_tot
            else
                ener_subt = 0.040
            endif
            en_cpr(phi_bin,eta_bin)=en_cpr(phi_bin,eta_bin)-ener_subt

C       Project Track back to the primary Vertex. Calculate Energy
C       of charged tracks and load energy into eta-phi grid at Vertex
            trak(1)  = egr_globtrk(ihit).r0                               ! r0
            trak(2)  = egr_globtrk(ihit).r0
     1               * egr_globtrk(ihit).phi0*deg_to_rad !r0*phi0
            trak(3)  = egr_globtrk(ihit).z0                               !z0
            trak(4)  = egr_globtrk(ihit).psi*deg_to_rad                   !psi
            trak(5)  = egr_globtrk(ihit).tanl                             !tanl
            trak(6)  = FLOAT(egr_globtrk(ihit).icharge)*
     +                 (0.2997*5.)*egr_globtrk(ihit).invpt/1000.        !q/R

c
C***********use this  to project back, trk1 holds the output
C       At present assume that primary vertex is at X = Y = Z = 0
cWJL
cWJL ---- can use located vertex from evr now!!!!   30-08-96
cWJL
            xp(1) = 0.0
            xp(2) = 0.0
            call ev0_circle_param_r(trak,xc1,r1)
            call ev0_project_track(xc1,r1,xp,xn1)
            call ev0_update_track_param(xc1,r1,xn1,trak,trk1) 

*    Calculate (phi,eta) bin for Track at vertex
            eta_hit = -1.0*ALOG(TAN(ATAN2(pt,pz)/2.0))
            if(eta_hit.gt.0.0)then                       ! + eta's in bins 21 -> 40.
               eta_trk = INT(20.0*eta_hit) + 21
               if(eta_trk.gt.40)eta_trk = 40
            else                                         ! - eta's in bins  1 -> 20
               eta_trk = 20 - INT(20.0*abs(eta_hit))
            endif              
            if(trk1(4).gt.0)then                         ! Phi angle from 0 -> 180 degrees
               phi_trk = INT(trk1(4)/(0.10472/2.0)) + 1
            else
               phi_trk = INT(trk1(4)/(0.10472/2.0)) + 120
            endif
            
            if(sum_down)then
             eta_trk = ((eta_trk-1)/2) + 1
             phi_trk = ((phi_trk-1)/2) + 1
            end if
             if(eta_trk.le.40.and.eta_trk.gt.0.and.
     1          phi_trk.le.120.and.phi_trk.gt.0) then
               e_vtx = sqrt(p_tot**2 + 0.13957**2)
               e_char(phi_trk,eta_trk) = e_char(phi_trk,eta_trk)+e_vtx
            end if  ! bound check 
          endif  ! track hits emc
        endif    ! good tracks
      enddo      ! End loop over input tracks

*     fill output table
      do i = 1,120            ! Loop over phi_bins
         do j = 1,40          ! Loop over eta_bins
            pbin = i
            ebin = j
            if(sum_down)then
             pbin = ((pbin-1)/2) + 1
             ebin = ((ebin-1)/2) + 1
            end if
            if(ems_emecor_h.nok.lt.ems_emecor_h.maxlen) then
             if(en_cpr(pbin,ebin).ne.0.0.or.
     +          e_char(pbin,ebin).gt.0.0.or.
     +          numc(pbin,ebin).ge.1.or.
     +          etot2(pbin,ebin).ne.0.)then
                  ems_emecor_h.nok = ems_emecor_h.nok + 1
                  out_num          = ems_emecor_h.nok
                  ems_emecor(out_num).eta_bin  = ebin
                  ems_emecor(out_num).phi_bin  = pbin
                  ems_emecor(out_num).eneut_mct= en_mct(pbin,ebin)
                  ems_emecor(out_num).eneut_cpr= cal*en_cpr(pbin,ebin)
                  ems_emecor(out_num).etot_2   = cal*en_mct(pbin,ebin)
     +                                         + e_char(pbin,ebin)
                  ems_emecor(out_num).etot_3   = etot3(pbin,ebin)
                  etots(pbin,ebin)             = e_char(pbin,ebin)
     +                                         + cal*en_cpr(pbin,ebin)
                  ems_emecor(out_num).etot_sim = etots(pbin,ebin)
                  ems_emecor(out_num).num_char = numc(pbin,ebin)
                  ems_emecor(out_num).echar_vert = e_char(pbin,ebin)
             endif       ! Output for phi - eta bin
            else
             write(*,*)
     1     'EMS_EGRID -- WARNING!!! ems_emecor is full - rows missed!'
             write(*,*)
     1     '          -- WARNING!!! increase .maxlen and redo...'
            endif          ! Number of output groups OK?
         enddo       ! Loop over eta bins
      enddo          ! Loop over phi bins

      EMS_EGRID=STAFCV_OK
999   RETURN
      END
