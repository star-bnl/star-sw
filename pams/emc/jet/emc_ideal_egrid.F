c:>------------------------------------------------------------------
C:FILE:         emc_ideal_egrid.F
C:DESCRIPTION:  EMC Analysis Module, package emc, STAF compliant.
c:AUTHOR:       OGAWA, Akio
C:BUGS:         None known.
C:HISTORY:      20Sep97-v1.0-akio- Creation.
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION EMC_IDEAL_EGRID(
     1         g2t_track_h,         g2t_track ,
     2       ems_control_h,       ems_control ,
     3        emc_energy_h,        emc_energy )
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMC_IDEAL_EGRID
C:DESCRIPTION:  g2t_track energy in (eta,phi) grids...
C:ARGUMENTS:    g2t_track ems_control
C:ARGUMENTS:    emc_energy
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE
C
#include "emc_ideal_egrid.inc"

c---energy grid id
      integer MAXP,MAXE
      parameter(MAXP=120, MAXE=80)
      real id(MAXP,MAXE)
c---local variables
      integer i,nout,mout,pid,pbin,ebin
      real eta,phi,e_vtx,pt,etot
c---keep variables
      COMMON/keep_ideal_egrid/ PI,PI2,deg_to_rad
      real PI,PI2,deg_to_rad

c---start executable statements...
      emc_ideal_egrid = STAFCV_OK     

c---initialization. Only once at first call, or if you want to change setting 
      PI           = ACOS(-1.0)
      PI2          = 2.0*PI
      deg_to_rad   = PI/180.0

C---- Initialize nok counter and variable arrays
      emc_energy_h.nok = 0
      nout = 0
      etot = 0.0
      call vzero(id,MAXP*MAXE)

C*- Load g2t_track energy
      do i=1,g2t_track_h.nok
         pid=g2t_track(i).ge_pid
         if(pid.le.0) goto 100 
         if(pid.ge.48) goto 100
         if(ems_control(1).ideal_jet_mode.ge.2)then
            if(pid.eq.4) goto 100
            if(ems_control(1).ideal_jet_mode.ge.3)then
               if(pid.eq.1) goto 100
               if(pid.eq.7) goto 100
               if(pid.eq.10) goto 100
               if(pid.eq.13) goto 100
               if(pid.eq.25) goto 100
            endif
         endif
C*-Calculate (phi,eta) bin for Track at vertex
         pt=sqrt(g2t_track(i).p(1)**2+g2t_track(i).p(2)**2)
         eta=-1.0*log(tan(atan2(pt,g2t_track(i).p(3))/2.0))
         ebin=int((eta-ems_control(1).chrg_eta_min)
     +        * float(ems_control(1).chrg_eta_nbin)
     +        /(ems_control(1).chrg_eta_max
     +        -ems_control(1).chrg_eta_min))+1            
         if(ebin.lt.1 .or. 
     +        ebin.gt.ems_control(1).chrg_eta_nbin)
     +        goto 100
         phi=atan2(g2t_track(i).p(2),g2t_track(i).p(1))
         do while(phi .ge. PI2)
            phi=phi-PI2
         enddo
         do while(phi .lt. 0.0)
            phi=phi+PI2
         enddo
         pbin=int(phi/PI2*float(ems_control(1).chrg_phi_nbin))+1
         e_vtx=g2t_track(i).e
         if(id(pbin,ebin).eq.0) then
            nout=nout+1
            if(nout. ge. emc_energy_h.maxlen)then
               write(*,*) 'EMC_GRID: Need more memory EMC_ENERGY'
               EMC_IDEAL_EGRID=STAFCV_BAD
               return
            endif
            eta=(ebin-0.5)
     +           /float(ems_control(1).chrg_eta_nbin)
     +           *(ems_control(1).chrg_eta_max
     +           -ems_control(1).chrg_eta_min)
     +           +ems_control(1).chrg_eta_min
            phi=(pbin-0.5)*PI2
     +           /float(ems_control(1).chrg_phi_nbin)
            id(pbin,ebin)=nout
            emc_energy(nout).det   =0
            emc_energy(nout).ieta  =ebin
            emc_energy(nout).iphi  =pbin
            emc_energy(nout).eta   =eta
            emc_energy(nout).phi   =phi
            emc_energy(nout).energy=e_vtx
            emc_energy(nout).error =0.0
            emc_energy(nout).n_track=1
         else
            mout=id(pbin,ebin)
            emc_energy(mout).energy =
     +           emc_energy(mout).energy + e_vtx
            emc_energy(mout).error  =
     +           emc_energy(mout).error  + 0.0
            emc_energy(mout).n_track=
     +           emc_energy(mout).n_track+ 1
         endif
         etot=etot+e_vtx
 100  continue
      enddo                     ! End loop over input tracks
      write(*,*) 'EMC_IDEAL_GRID done : ',nout,' wrote out' 
      emc_energy_h.nok = nout
999   RETURN
      END


