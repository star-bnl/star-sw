c:>------------------------------------------------------------------
C:FILE:         emc_egrid.F
C:DESCRIPTION:  EMC Analysis Module, package emc, STAF compliant.
c:AUTHOR:       OGAWA, Akio
c:AUTHOR:       Originaly by W.J. Llope, Rice University
c:AUTHOR:       TAS Version by B. Christie
C:BUGS:         None known.
C:HISTORY:      3Sep97-v1.0-akio- Creation.
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION EMC_EGRID(
     1     ems_hits_bemc_h,     ems_hits_bemc ,
     2     ems_hits_eemc_h,     ems_hits_eemc ,
     3       egr_globtrk_h,       egr_globtrk ,
     4       ems_control_h,       ems_control ,
     5        emc_energy_h,        emc_energy )
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMC_EGRID
C:DESCRIPTION:  charged and neutral energy in (eta,phi) grids...
C     Takes in the Table that contains the Total signal in the 
C     EMC barrel and endcap towers, the table of the primary vertex
C     info.,and the Global track info. Projects the global tracks to the
C     EMC, calculates the most probable energy deposited in the tower/towers
C     by the track, subtracts the energy due to charged track from the
C     energy in the tower, and puts out resulting tower signals (due to
C     neutrals) in e_n table. This module then projects the tracks
C     back to the primary vertex, claculates the Energy, eta, and phi
C     for the tracks and loads a table with the charged energy at the
C     vertex.
C:ARGUMENTS:    ems_hits_bemc ems_hits_eemc egr_globtrk ems_control
C:ARGUMENTS:    emc_energy
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE
C
#include "emc_egrid.inc"

c---variables for control
      logical bemc_on, eemc_on
      INTEGER control_ivid(2)/10,1/  
      INTEGER ivid, control_chid(2)                 
c---energy grid id
      integer MAXR,MAXP,MAXE
      parameter(MAXR=4, MAXP=120, MAXE=80)
      real id(MAXR,MAXP,MAXE)
c---local variables
      integer i,j,k,nout,mout,itwr,ihit,i_flag,nphi(3),neta(3)
      integer eta_bin,phi_bin,rl,phi_trk,eta_trk,pbin,ebin
      real trk_qual_cut,b_field,cal,trk_qual,r_int
      real trkchar,pt,pz,p_tot,crad,x_strt,y_strt
      real cap_phi0,xc,yc,p_input(6),z_int,rval,rval1,rval2
      real zval,ang_int,td_ang,cosphi_int,int_yn,dirvec(3)
      real eta_hit,phi_hit,ene_sub,err_sub,sign
      real trak(6),trk1(6),xc1(2),r1,xp(2),xn1(2)
      real eta,phi,e_vtx,diff,ener_subt,etot
c---keep variables
      COMMON/keep/PI,PI2,deg_to_rad
      real PI,PI2,deg_to_rad

c---start executable statements...
      emc_egrid = STAFCV_OK     

c---initialization. Only once at first call, or if you want to change setting 
      PI           = ACOS(-1.0)
      PI2          = 2.0*PI
      deg_to_rad   = PI/180.0
      trk_qual_cut = ems_control(1).trk_qual_cut
      b_field      = ems_control(1).b_field
      cal          = ems_control(1).sampfrac
      ivid=mod(ems_control(1).onoff_control,100)
      do i = 1,2
         control_chid(i) = ivid/control_ivid(i)
         ivid = ivid - control_chid(i)*control_ivid(i)
      end do 
      if(ivid.ne.0)then
         write(*,*)'EMC_EGRID:control decoding error',control_ivid
      end if
      if(control_chid(1).eq.1) then
         bemc_on=.false.
      else
         bemc_on=.true.
      endif
      if(control_chid(2).eq.1) then
         eemc_on=.false.
      else
         eemc_on=.true.
      endif

C---- Initialize nok counter and variable arrays
      emc_energy_h.nok = 0
      nout = 0
      etot = 0.0
      call vzero(id,MAXR*MAXP*MAXE)
      nphi(1)=ems_control(1).eemc_phi_nbin
      nphi(2)=nphi(1)
      nphi(3)=ems_control(1).bemc_phi_nbin
      neta(1)=ems_control(1).eemc_eta_nbin
      neta(2)=neta(1)
      neta(3)=ems_control(1).bemc_eta_nbin

C*- Load EMC energy to neutral energy
      if(bemc_on) then
         if(ems_hits_bemc_h.nok. ge. emc_energy_h.maxlen)then
            write(*,*) 'EMC_GRID:Need more memory:EMC_ENERGY'
            EMC_EGRID=STAFCV_BAD
            return
         endif
         rl=3
         do itwr = 1,ems_hits_bemc_h.nok
            if(ems_hits_bemc(itwr).tower_dep.eq.0)then  !tower sum in depth=0...
               pbin=ems_hits_bemc(itwr).phi_bin
               ebin=ems_hits_bemc(itwr).eta_bin
               nout=nout+1
               id(rl,pbin,ebin)=nout                  
               call find_pos_emc(ems_control,rl,pbin,ebin,phi,eta)
               emc_energy(nout).rl    =rl
               emc_energy(nout).ieta  =ebin
               emc_energy(nout).iphi  =pbin
               emc_energy(nout).eta   =eta
               emc_energy(nout).phi   =phi
               emc_energy(nout).energy=cal*ems_hits_bemc(itwr).e_sum
               emc_energy(nout).error =0.0
               emc_energy(nout).n_track=1000
               etot=etot+cal*ems_hits_bemc(itwr).e_sum
            end if
         enddo
      endif
c*- and for Endcap
      if(eemc_on) then
         if(nout+ems_hits_eemc_h.nok .ge. emc_energy_h.maxlen)then
            write(*,*) 'EMC_GRID:Need more memory:EMC_ENERGY'
            EMC_EGRID=STAFCV_BAD
            return
         endif
         do itwr = 1,ems_hits_eemc_h.nok
            if(ems_hits_eemc(itwr).tower_dep.eq.0)then ! tower sum in depth=0...             
               rl  =ems_hits_eemc(itwr).rl
               pbin=ems_hits_eemc(itwr).tower_phi_bin
               ebin=ems_hits_eemc(itwr).tower_eta_bin
               nout=nout+1
               id(rl,pbin,ebin)=nout     
               call find_pos_emc(ems_control,rl,pbin,ebin,phi,eta)
               emc_energy(nout).rl    =rl
               emc_energy(nout).ieta  =ebin
               emc_energy(nout).iphi  =pbin
               emc_energy(nout).eta   =eta
               emc_energy(nout).phi   =phi
               emc_energy(nout).energy=cal*ems_hits_eemc(itwr).eb_sum
               emc_energy(nout).error =0.0
               emc_energy(nout).n_track=1000
               etot=etot+cal*ems_hits_eemc(itwr).eb_sum
            end if
         enddo
      endif

C*- get the tracks from GLOBTRK table
      write(*,*)'EMC_EGRID -- No. tracks in =',egr_globtrk_h.nok
      do ihit = 1, egr_globtrk_h.nok       ! Loop over input tracks
         trk_qual=egr_globtrk(ihit).chisq/float(egr_globtrk(ihit).ndegf)
         if(trk_qual.le.trk_qual_cut)then
            trkchar = float(egr_globtrk(ihit).icharge)
            pt      = 1.0/egr_globtrk(ihit).invpt
            pz      = pt*egr_globtrk(ihit).tanl
            p_tot   = sqrt(pt**2 + pz**2)
            crad    = pt/abs(trkchar*0.0003*b_field)
            x_strt  = egr_globtrk(ihit).r0
     1              * cos(egr_globtrk(ihit).phi0*deg_to_rad)
            y_strt  = egr_globtrk(ihit).r0
     1              * sin(egr_globtrk(ihit).phi0*deg_to_rad)
            cap_phi0= egr_globtrk(ihit).psi*deg_to_rad+trkchar*0.5*pi
            xc      = x_strt - crad*cos(cap_phi0)
            yc      = y_strt - crad*sin(cap_phi0)
            p_input(1) = xc
            p_input(2) = yc
            p_input(3) = egr_globtrk(ihit).z0
            p_input(4) = crad
            p_input(5) = egr_globtrk(ihit).tanl 
            p_input(6) = cap_phi0
            z_int = 10000.
c*-Flag for track which go through
c*-barrel=1, endcap=2, non=0
            i_flag=0  
c*-Barrel
            if(bemc_on)then
               rval  = ems_control(1).bemc_inner_r
               zval  = ems_control(1).bemc_max_z
               call eut_extra_rad(p_input,zval,trkchar,ang_int,
     +              td_ang,cosphi_int,z_int,int_yn,dirvec)          
               if(int_yn.eq.0.and.abs(z_int).le.zval)then ! Trk hits EMC Barrel?
                  i_flag=1
                  eta_hit = -1.0*log(tan(atan2(rval,z_int)/2.0))
                  rl=3
               endif
            endif
c*-Endcap
            if(eemc_on .and. i_flag.eq.0)then
               rval1 = ems_control(1).eemc_inner_r
               rval2 = ems_control(1).eemc_outer_r
               sign  = pz/abs(pz)
               rl    = (3-sign)/2
               zval  = sign*ems_control(1).eemc_inner_z
               call eut_extra_z(p_input,zval,trkchar,
     1              ang_int,td_ang,r_int,int_yn,dirvec)          
               if(int_yn.eq.0 .and. 
     1              r_int.le.rval2.and.r_int.gt.rval1) then ! Trk hits EMC Endcap?
                  i_flag=2
                  eta_hit = -1.0*log(tan(atan2(r_int,zval)/2.0))
               endif
            endif
c*-Fill tables            
            if(i_flag.gt.0) then
               call find_bin_emc
     +              (ems_control,rl,ang_int,eta_hit,pbin,ebin)
               call pion_energy_deposit(rl,p_tot,ene_sub,err_sub)
               if(id(rl,pbin,ebin).eq.0) then
                  nout=nout+1
                  if(nout. ge. emc_energy_h.maxlen)then
                     write(*,*)'EMC_GRID:Need more memory:EMC_ENERGY'
                     EMC_EGRID=STAFCV_BAD
                     return
                  endif
                  id(rl,pbin,ebin)=nout
                  call find_pos_emc(ems_control,rl,pbin,ebin,phi,eta)
                  emc_energy(nout).rl    =rl
                  emc_energy(nout).ieta  =ebin
                  emc_energy(nout).iphi  =pbin
                  emc_energy(nout).eta   =eta
                  emc_energy(nout).phi   =phi
                  emc_energy(nout).energy=-cal*ene_sub
                  emc_energy(nout).error =cal*err_sub
                  emc_energy(nout).n_track=1
               else
                  mout=id(rl,pbin,ebin)
                  emc_energy(mout).energy =
     +                 emc_energy(mout).energy - cal*ene_sub
                  emc_energy(mout).error  =
     +                 emc_energy(mout).error  + cal*ene_sub
                  emc_energy(mout).n_track=
     +                 emc_energy(mout).n_track  + 1
               endif
               etot=etot-cal*ene_sub
            endif
            
C*-Project Track back to the primary Vertex. Calculate Energy
C*-of charged tracks and load energy into eta-phi grid at Vertex
            trak(1)  = egr_globtrk(ihit).r0 ! r0
            trak(2)  = egr_globtrk(ihit).r0
     1               * egr_globtrk(ihit).phi0*deg_to_rad !r0*phi0
            trak(3)  = egr_globtrk(ihit).z0 !z0
            trak(4)  = egr_globtrk(ihit).psi*deg_to_rad !psi
            trak(5)  = egr_globtrk(ihit).tanl !tanl
            trak(6)  = FLOAT(egr_globtrk(ihit).icharge)
     +               * (0.2997*5.)*egr_globtrk(ihit).invpt/1000.        !q/R

C*-At present assume that primary vertex is at X = Y = Z = 0
C*- Shall we use vertex position? If yes, how do we treat 
C*- eta,phi for EMC energy grid? Need to improve! -akio
            xp(1) = 0.0
            xp(2) = 0.0
            
            call ev0_circle_param_r(trak,xc1,r1)
            call ev0_project_track(xc1,r1,xp,xn1)
            call ev0_update_track_param(xc1,r1,xn1,trak,trk1) 
            
C*-Calculate (phi,eta) bin for Track at vertex
            rl=4
            eta=-1.0*ALOG(TAN(ATAN2(pt,pz)/2.0))
            ebin=int((eta-ems_control(1).chrg_eta_min)
     +          * float(ems_control(1).chrg_eta_nbin)
     +          /(ems_control(1).chrg_eta_max
     +           -ems_control(1).chrg_eta_min))+1            
            if(ebin.lt.1 .or. 
     +         ebin.gt.ems_control(1).chrg_eta_nbin)
     +         goto 100
            phi=trk1(4)
            do while(phi .ge. PI2)
               phi=phi-PI2
            enddo
            do while(phi .lt. 0.0)
               phi=phi+PI2
            enddo
            pbin=int(phi/PI2*float(ems_control(1).chrg_phi_nbin))+1
c            write(*,*) phi,eta,pbin,ebin
            e_vtx=sqrt(p_tot**2 + 0.13957**2)        !assume pion
            if(id(rl,pbin,ebin).eq.0) then
               nout=nout+1
               if(nout. ge. emc_energy_h.maxlen)then
                  write(*,*) 'EMC_GRID: Need more memory EMC_ENERGY'
                  EMC_EGRID=STAFCV_BAD
                  return
               endif
               eta=(ebin-0.5)
     +            /float(ems_control(1).chrg_eta_nbin)
     +            *(ems_control(1).chrg_eta_max
     +            -ems_control(1).chrg_eta_min)
     +            +ems_control(1).chrg_eta_min
               phi=(pbin-0.5)*PI2
     +            /float(ems_control(1).chrg_phi_nbin)
               id(rl,pbin,ebin)=nout
               emc_energy(nout).rl    =rl
               emc_energy(nout).ieta  =ebin
               emc_energy(nout).iphi  =pbin
               emc_energy(nout).eta   =eta
               emc_energy(nout).phi   =phi
               emc_energy(nout).energy=e_vtx
               emc_energy(nout).error =0
               emc_energy(nout).n_track=1
            else
               mout=id(rl,pbin,ebin)
               emc_energy(mout).energy =
     +              emc_energy(mout).energy + e_vtx
               emc_energy(mout).error  =
     +              emc_energy(mout).error  + 0.0
               emc_energy(mout).n_track=
     +              emc_energy(mout).n_track+ 1
            endif
            etot=etot+e_vtx
         endif                  ! good tracks
 100     continue
      enddo                     ! End loop over input tracks
      
c*-Check negative cell and subtract from suround 8 cells.
      do k=1, nout
       if(emc_energy(k).energy. lt. 0.0)then
        ener_subt=-emc_energy(k).energy
        do i=-1,1
         pbin=emc_energy(k).iphi+i
         rl=emc_energy(k).rl
         if(pbin.lt.1)          pbin=nphi(rl)
         if(pbin.gt.nphi(rl)) pbin=1
         do j=-1,1
          if(i.ne.0 .or. j.ne.0) then
           ebin=emc_energy(k).ieta+j                  
           if(ebin.ge.1 .and. ebin.le.neta(rl)) then
            mout=id(rl,pbin,ebin)
            if(mout.ne.0) then
             diff=emc_energy(mout).energy-ener_subt
             if(diff.ge.0.0) then
               emc_energy(mout).energy = diff
               emc_energy(mout).n_track=emc_energy(mout).n_track+1
               emc_energy(k).energy = 0.0
               goto 99
             else
               ener_subt=-diff
               emc_energy(mout).energy=0.0
               emc_energy(mout).n_track=emc_energy(mout).n_track+1
             endif
            endif
           endif
          endif
         enddo
        enddo
        emc_energy(k).energy = -ener_subt
       endif
 99    continue
      enddo     

c*- Test
c      do ebin=1,20
c         call find_pos_emc(ems_control,3,1,ebin,phi,eta)
c         write(*,*) 'find_pos/b/eta; ', ebin, eta 
c      enddo
c      do ebin=1,12
c         call find_pos_emc(ems_control,1,1,ebin,phi,eta)
c         write(*,*) 'find_pos/e/eta; ', ebin, eta 
c      enddo
c      do pbin=1,60
c         call find_pos_emc(ems_control,3,pbin,1,phi,eta)
c         write(*,*) 'find_pos/b/phi; ', pbin, phi
c      enddo
c      do pbin=1,60
c         call find_pos_emc(ems_control,1,pbin,1,phi,eta)
c         write(*,*) 'find_pos/e/phi; ', pbin, phi 
c      enddo
c      rl=120
c      do i=1,rl
c        eta=float(i)/float(rl)*2.0-1.0
c         call find_bin_emc(ems_control,3,0.0,eta,pbin,ebin)
c         write(*,*) 'find_bin/b/eta; ', eta, ebin
c      enddo
c      do i=1,rl
c         eta=float(i)/float(rl)*4.0-2.0
c         call find_bin_emc(ems_control,1,0.0,eta,pbin,ebin)
c         write(*,*) 'find_bin/e/eta; ', eta, ebin
c      enddo
c      rl=240
c      do i=1,rl
c         phi=float(i)/float(rl)*PI2
c         call find_bin_emc(ems_control,3,phi,0.0,pbin,ebin)
c         write(*,*) 'find_bin/b/phi; ', phi, pbin
c      enddo
c      do i=1,rl
c         phi=float(i)/float(rl)*PI2
c         call find_bin_emc(ems_control,1,phi,1.1,pbin,ebin)
c         write(*,*) 'find_bin/e/phi; ', phi, pbin
c      enddo

      write(*,*) 'EMC_GRID done : ',nout,' wrote out' 
      write(*,*) 'EMC_GRID done : Total energy : ', etot
      emc_energy_h.nok = nout
      EMC_EGRID=STAFCV_OK
999   RETURN
      END
