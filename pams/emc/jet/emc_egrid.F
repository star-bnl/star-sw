c:>------------------------------------------------------------------
C:FILE:         emc_egrid.F
C:DESCRIPTION:  EMC Aalysis Module, package emc, STAF compliant.
c:AUTHOR:       OGAWA, Akio
c:AUTHOR:       Originaly by W.J. Llope, Rice University
c:AUTHOR:       TAS Version by B. Christie
C:BUGS:         None known.
C:HISTORY:      3Sep97-v1.0-akio- Creation.
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION EMC_EGRID(
     1     emc_hits_bemc_h,     emc_hits_bemc ,
     2     emc_hits_eemc_h,     emc_hits_eemc ,
     3         dst_track_h,         dst_track ,
     4       ems_control_h,       ems_control ,
     5     emc_egrid_par_h,     emc_egrid_par ,
     6        emc_energy_h,        emc_energy )
      IMPLICIT NONE
#include "emc_def.h"
#include "math_constants.inc"
#include "phys_constants.inc"
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
C:ARGUMENTS:    emc_hits_bemc emc_hits_eemc dst_track ems_control
C:ARGUMENTS:    emc_energy
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
#include "emc_egrid.inc"

c---energy grid id
      integer MAXID,MAXM,MAXE,MAXS
      parameter(MAXID=3, MAXM=120, MAXE=20, MAXS=5)
      real id(MAXID,MAXM,MAXE,MAXS)
c---local variables
      integer detid(0:MAXDET)
      integer i,j,k,nout,mout,itwr,ihit,mod,sub,idet
      integer eta_bin,phi_bin,det,phi_trk,eta_trk,pbin,ebin
      real trk_qual_cut,b_field,trk_qual,r_int,emin,emax
      real trkchar,pt,pz,p_tot,crad,x_strt,y_strt
      real cap_phi0,xc,yc,p_input(6),z_int,rval,rval1,rval2
      real zval,ang_int,td_ang,cosphi_int,int_yn,dirvec(3)
      real eta_hit,phi_hit,ene_sub,err_sub,sign
      real trak(6),trk1(6),xc1(2),r1,xp(2),xn1(2)
      real eta,phi,e_vtx,diff,ener_subt,etot
      real pos(3) /0.0, 0.0, 0.0/
      real mfld(3)

c---start executable statements...
      emc_egrid = STAFCV_OK     

      trk_qual_cut = ems_control(1).trk_qual_cut
      call gufld(pos,mfld)
      b_field=mfld(3)
      
      do i=0,MAXDET
         detid(i)=-2
      enddo
      detid(0)=1
      detid(BEMC)=2
      detid(EEMC)=3

C---- Initialize nok counter and variable arrays
      emc_energy_h.nok = 0
      nout = 0
      etot = 0.0
      call vzero(id,MAXID*MAXM*MAXE*MAXS)
      emin=emc_egrid_par(1).chrg_eta_min
      emax=emc_egrid_par(1).chrg_eta_max
      if(emin.lt.-2.0) then
         write(*,*)'emc_egrid: chrg_eta_min should be >-2.0 Ignored' 
         emin=-2.0
      endif
           if(emin.gt.2.0) then
         write(*,*)'emc_egrid: chrg_eta_max should be <2.0 Ignored'
         emax=2.0
      endif
          
C*- Load Barrel EMC energy
      write(*,*)'EMC_EGRID : No. barrel hits in =',emc_hits_bemc_h.nok
      if(ems_control(1).bemc_on.eq.1) then
         idet=2
         do itwr = 1,emc_hits_bemc_h.nok
            if(emc_hits_bemc(itwr).det .eq. BEMC) then
               nout=nout+1
               if(nout .gt. emc_energy_h.maxlen)then
                  write(*,*) '***EMC_EGRID:Need more memory in EMC_ENERGY'
                  EMC_EGRID=STAFCV_BAD
                  return
               endif 
               mod =emc_hits_bemc(itwr).module
               ebin=emc_hits_bemc(itwr).eta
               sub =emc_hits_bemc(itwr).sub
               id(idet,mod,ebin,sub)=nout                  
               call find_pos_emc
     +              (ems_control,BEMC,mod,ebin,sub,phi,eta)
               emc_energy(nout).det       =BEMC
               emc_energy(nout).module    =mod
               emc_energy(nout).ebin      =ebin
               emc_energy(nout).sub       =sub
               emc_energy(nout).eta       =eta
               emc_energy(nout).phi       =phi
               emc_energy(nout).energy    =emc_hits_bemc(itwr).energy
               emc_energy(nout).energy_twr=emc_hits_bemc(itwr).energy
               emc_energy(nout).energy_trk=0.0
               emc_energy(nout).energy_chg=0.0
               emc_energy(nout).error     =0.0
               emc_energy(nout).n_track   =0
               emc_energy(nout).n_charge  =0
               etot=etot+emc_hits_bemc(itwr).energy
            end if
         enddo
      endif
c*- and for Endcap
      if(ems_control(1).eemc_on.eq.1) then
         idet=3
         write(*,*)'EMC_EGRID : No. endcap hits in =',emc_hits_eemc_h.nok
         do itwr = 1,emc_hits_eemc_h.nok
            if(emc_hits_eemc(itwr).det .eq. EEMC) then
               nout=nout+1
               if(nout .gt. emc_energy_h.maxlen)then
                  write(*,*) '***EMC_EGRID:Need more memory in EMC_ENERGY'
                  EMC_EGRID=STAFCV_BAD
                  return
               endif
               mod =emc_hits_eemc(itwr).module
               ebin=emc_hits_eemc(itwr).eta
               sub =emc_hits_eemc(itwr).sub
               id(idet,mod,ebin,sub)=nout     
               call find_pos_emc
     +              (ems_control,EEMC,mod,ebin,sub,phi,eta)
               emc_energy(nout).det       =EEMC
               emc_energy(nout).module    =mod
               emc_energy(nout).ebin      =ebin
               emc_energy(nout).sub       =sub
               emc_energy(nout).eta       =eta
               emc_energy(nout).phi       =phi
               emc_energy(nout).energy    =emc_hits_eemc(itwr).energy
               emc_energy(nout).energy_twr=emc_hits_eemc(itwr).energy
               emc_energy(nout).energy_trk=0.0
               emc_energy(nout).energy_chg=0.0
               emc_energy(nout).error     =0.0
               emc_energy(nout).n_track   =0
               emc_energy(nout).n_charge  =0
               etot=etot+emc_hits_eemc(itwr).energy
            end if
         enddo
      endif

C*- get tracks from DST_TRACK table
      write(*,*)'EMC_EGRID : No. tracks in =',dst_track_h.nok
      do ihit = 1, dst_track_h.nok       ! Loop over input tracks
         trk_qual=(dst_track(ihit).chisq(1)+
     +             dst_track(ihit).chisq(2))/2.0
         if(trk_qual.le.trk_qual_cut)then
            trkchar = float(dst_track(ihit).icharge)
            pt      = 1.0/dst_track(ihit).invpt
            pz      = pt*dst_track(ihit).tanl
            p_tot   = sqrt(pt**2 + pz**2)
            crad    = pt/abs(trkchar*C_D_CURVATURE*b_field)
            x_strt  = dst_track(ihit).r0
     +           *cos(dst_track(ihit).phi0*C_RAD_PER_DEG)
            y_strt  = dst_track(ihit).r0
     +           *sin(dst_track(ihit).phi0*C_RAD_PER_DEG)
            cap_phi0= dst_track(ihit).psi*C_RAD_PER_DEG+trkchar*C_PI_2
            xc      = x_strt - crad*cos(cap_phi0)
            yc      = y_strt - crad*sin(cap_phi0)
            p_input(1) = xc
            p_input(2) = yc
            p_input(3) = dst_track(ihit).z0
            p_input(4) = crad
            p_input(5) = dst_track(ihit).tanl 
            p_input(6) = cap_phi0
            z_int = 10000.

            det=0  
c*-Check if it hits Barrel
            if(ems_control(1).bemc_on.eq.1)then
               rval  = ems_control(1).bemc_inner_r
               zval  = ems_control(1).bemc_max_z
               call eut_extra_rad(p_input,zval,trkchar,ang_int,
     +              td_ang,cosphi_int,z_int,int_yn,dirvec)          
               if(int_yn.eq.0.and.abs(z_int).le.zval)then ! Trk hits EMC Barrel?
                  eta_hit = -1.0*log(tan(atan2(rval,z_int)/2.0))
                  det=BEMC
               endif
            endif
c*-or Endcap
            if(ems_control(1).eemc_on.eq.1 .and. det.eq.0)then
               rval1 = ems_control(1).eemc_inner_r
               rval2 = ems_control(1).eemc_outer_r
               sign  = pz/abs(pz)
               zval  = sign*ems_control(1).eemc_inner_z
               call eut_extra_z(p_input,zval,trkchar,
     1              ang_int,td_ang,r_int,int_yn,dirvec)          
               if(int_yn.eq.0 .and. 
     1              r_int.le.rval2.and.r_int.gt.rval1) then ! Trk hits EMC Endcap?
                  det=EEMC
                  eta_hit = -1.0*log(tan(atan2(r_int,zval)/2.0))
               endif
            endif
c*-Subtract expected energy deposit from charged track
            if(det.gt.0) then
               ene_sub=0.0
               err_sub=0.0
               call find_bin_emc
     +              (ems_control,ang_int,eta_hit,det,mod,ebin,sub)
               if(det.gt.0) then
                  call pion_energy_deposit(det,p_tot,ene_sub,err_sub)
                  idet=detid(det)
               else
                  goto 10    ! the track didn't hit emc
               endif
               if(id(idet,mod,ebin,sub).eq.0) then ! No EMC hits for track,
                  nout=nout+1                      ! then give negative energy
*
                  if(nout .gt. emc_energy_h.maxlen)then
                     write(*,*) '***EMC_EGRID:Need more memory in EMC_ENERGY'
                     EMC_EGRID=STAFCV_BAD
                     return
                  endif
                  id(idet,mod,ebin,sub)=nout
                  call find_pos_emc
     +                 (ems_control,det,mod,ebin,sub,phi,eta)
                  emc_energy(nout).det       =det
                  emc_energy(nout).module    =mod
                  emc_energy(nout).ebin      =ebin
                  emc_energy(nout).sub       =sub
                  emc_energy(nout).eta       =eta
                  emc_energy(nout).phi       =phi
                  emc_energy(nout).energy    =-ene_sub
                  emc_energy(nout).energy_twr=0.0
                  emc_energy(nout).energy_trk=0.0
                  emc_energy(nout).energy_chg=ene_sub
                  emc_energy(nout).error     =err_sub
                  emc_energy(nout).n_track   =0
                  emc_energy(nout).n_charge  =1
               else
                  mout=id(idet,mod,ebin,sub)
                  emc_energy(mout).energy =
     +                 emc_energy(mout).energy - ene_sub
                  emc_energy(mout).energy_chg =
     +                 emc_energy(mout).energy_chg + ene_sub
                  emc_energy(mout).error  =
     +                 emc_energy(mout).error  + err_sub     !??? temporaly
                  emc_energy(mout).n_charge=
     +                 emc_energy(mout).n_charge  + 1
               endif
               etot=etot-ene_sub
            endif
 10         continue

C*-Project Tracks back to the primary Vertex. Calculate Energy
C*-of charged tracks and load energy into eta-phi grid at Vertex
            trak(1)  = dst_track(ihit).r0
            trak(2)  = trak(1)*dst_track(ihit).phi0   
     +           *C_RAD_PER_DEG !r0*phi0
            trak(3)  = dst_track(ihit).z0 !z0
            trak(4)  = dst_track(ihit).psi*C_RAD_PER_DEG !psi
            trak(5)  = dst_track(ihit).tanl !tanl
            trak(6)  = FLOAT(dst_track(ihit).icharge)
     +               * (0.2997*5.)*dst_track(ihit).invpt/1000.        !q/R

C*-At present assume that primary vertex is at X = Y = Z = 0
C*- Shall we use vertex position? If yes, how do we treat 
C*- phi,eta for EMC energy grid? Need to improve! -akio
            xp(1) = 0.0
            xp(2) = 0.0
            
            call emc_circle_param_r(trak,xc1,r1)
            call emc_project_track(xc1,r1,xp,xn1)
            call emc_update_track_param(xc1,r1,xn1,trak,trk1) 
            
C*-Calculate (phi,eta) bin for Track at vertex            
            eta=-1.0*ALOG(TAN(ATAN2(pt,pz)/2.0))
            if(eta.lt.emin .or. eta.gt.emax) goto 100
            e_vtx=sqrt(p_tot**2 + M_PION_PLUS**2) !assume pion
            phi=trk1(4)
            do while(phi .ge. C_2PI)
               phi=phi-C_2PI
            enddo
            do while(phi .lt. 0.0)
               phi=phi+C_2PI
            enddo
            call find_bin_emc
     +           (ems_control,phi,eta,det,mod,ebin,sub)
            if(det.lt.0) then
               goto 100
            else
               idet=detid(det)
            endif
            if(id(idet,mod,ebin,sub).eq.0) then ! No EMC hits for track. 
               nout=nout+1
               if(nout. gt. emc_energy_h.maxlen)then
                  write(*,*)'***EMC_GRID:Need more memory in EMC_ENERGY'
                  EMC_EGRID=STAFCV_BAD
                  return
               endif
               id(idet,mod,ebin,sub)=nout
               call find_pos_emc
     +              (ems_control,det,mod,ebin,sub,phi,eta)
               emc_energy(nout).det       =det
               emc_energy(nout).module    =mod
               emc_energy(nout).ebin      =ebin
               emc_energy(nout).sub       =sub
               emc_energy(nout).eta       =eta
               emc_energy(nout).phi       =phi
               emc_energy(nout).energy    =e_vtx
               emc_energy(nout).energy_twr=0.0
               emc_energy(nout).energy_trk=e_vtx
               emc_energy(nout).energy_chg=0.0
               emc_energy(nout).error     =0.0
               emc_energy(nout).n_track   =1
               emc_energy(nout).n_charge  =0
            else
               mout=id(idet,mod,ebin,sub)
               emc_energy(mout).energy =
     +              emc_energy(mout).energy + e_vtx 
               emc_energy(mout).energy_trk =
     +              emc_energy(mout).energy_trk + e_vtx
               emc_energy(mout).n_track =
     +              emc_energy(mout).n_track + 1
            endif
            etot=etot+e_vtx
         endif                  ! good tracks
 100     continue
      enddo                     ! End loop over input tracks
c      
      write(*,*) 'EMC_EGRID done :',nout,' grid wrote out' 
      write(*,*) 'EMC_EGRID done : Total energy = ', etot
      emc_energy_h.nok = nout
      EMC_EGRID=STAFCV_OK
999   RETURN
      END


