      INTEGER*4 FUNCTION EMS_ERJ(
     1       ems_control_h,       ems_control ,
     2        ems_emecor_h,        ems_emecor ,
     3        ems_jetpar_h,        ems_jetpar ,
     4       ems_jetlist_h,       ems_jetlist ) 
c:>------------------------------------------------------------------
C:FILE:         ems_erj.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       W.J. Llope, Rice University (TAS Version by B. Christie)
C:BUGS:         None known.
C:HISTORY:      31jul96-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMS_ERJ
C:DESCRIPTION:  Jet-finder, applies UA1-like algorithm from LUND JETSET.
C:ARGUMENTS:    ems_control ems_emecor ems_jetpar
C:ARGUMENTS:    ems_jetlist
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE

#include "ems_erj.inc"

      INTEGER ihit,test,nok,ierror
      INTEGER ierr
      INTEGER i,j
      INTEGER eta_pm,phi_bin,eta_bin,out_num,num_in
      INTEGER etab,phib,ntower_in
      common/hadr/xyz(10000,3),p_hadr(10000,5),de(5,5),q_hadr(10000),
     &            m_hadr(10000),absp0,px0,py0,pz0,eta0,ph0,th0
      common/jets/njet, ns_cells(100), p_jet(100,5), et_tot(120,40)
      integer njet, i_jet, ns_cells
      integer nc, eta_b,phi_b, ijet_bar,njet_bar,pid
      integer nch
      integer imark(100),njsum,nvsum,nmark,k,ik,marki,markj
      integer npart_in,ip_n,ip,ip_l,ip_s
      real phil,phis,dphi
      real p_hadr,de_hadr ,xyz,q_hadr,m_hadr,p_jet
      real absp, absr, et_hadr(120,40), pt,px,py,pz,ej
      real pi,twopi
      real eta,phi,theta,theta_grav,oo1,oo2
      real eta_grav,phi_grav,e_t_jet,calib,et_tot,et_elm(120,40)
      real absp0,px0,py0,pz0,ph0,eta0,th0,de
      real ej_min,cone_rad,et_seed
      integer kk,l, i_opt,init
      logical ok, ok_init
      logical weighted
      data njsum,nvsum/0,0/
      LOGICAL sum_down/.FALSE./

        data init/1/

c---------------------------------------------------------------------
C       Initialize Jet parameters
c---------------------------------------------------------------------
      if(init.eq.1)then  ! Only do this once
        write(*,*)'EMS_ERJ -- The jet finding parameters are:'
        write(*,*)
     1  'EMS_ERJ -- Jet Finding Method                              =',
     1    ems_jetpar(1).jetmethod
        write(*,*)
     1  'EMS_ERJ -- Enable eta,phi Weighting?                       =',
     1    ems_jetpar(1).jetweighting
        write(*,*)
     1  'EMS_ERJ -- Cone Radius                                     =',
     1    ems_jetpar(1).cone_rad
        write(*,*)
     1  'EMS_ERJ -- Min. energy for post-algorithm selection        =',
     1    ems_jetpar(1).ej_min
        write(*,*)
     1  'EMS_ERJ -- Min. seed tower energy in cluster algorithm     =',
     1    ems_jetpar(1).et_seed
        write(*,*)
     1  'EMS_ERJ -- Min. tower energy to be included in cluster sum =',
     1    ems_jetpar(1).et_min
        write(*,*)
     1  'EMS_ERJ -- Maximum phi deviation for back-to-back jets     =',
     1    ems_jetpar(1).phi_dev
        init = 0
        pi    = ACOS(-1.0)
        twopi = 2.0*pi
      endif
      nvsum = nvsum+1
      ej_min = ems_jetpar(1).ej_min

c---------------------------------------------------------------------
C     Initialize nok counter and variable arrays
c---------------------------------------------------------------------
      ems_erj = STAFCV_OK
      ems_jetlist_h.nok = 0
      out_num = 0
      num_in = 0
      do i=1,10000
           m_hadr(i) = 0.0
        do j=1,5
           p_hadr(i,j) = 0.0
           if(j .le. 3) xyz(i,j)    = 0.0
        enddo
      enddo
      sum_down = .FALSE.
      if(ems_control(1).bemc_Ntowers.eq.1200)
     1   sum_down = .TRUE.

c---------------------------------------------------------------------
C       Initialize Energy Grid
c---------------------------------------------------------------------
       do i = 1,120      ! Phi Bins
          do j = 1,40    ! Eta Bins
                et_tot(i,j) = 0.0
          enddo
       enddo

c---------------------------------------------------------------------
C      Load Energy Grid for Jet Finder
c---------------------------------------------------------------------
      ntower_in = ems_emecor_h.nok
      do i      = 1,ntower_in
        etab    = ems_emecor(i).eta_bin
        phib    = ems_emecor(i).phi_bin
        if(sum_down)then
         eta     = -0.950 + (float(etab)-1.)*0.10
        else
         eta     = -0.975 + (float(etab)-1.)*0.05
        end if
        et_tot(phib,etab) = ems_emecor(i).etot_sim/cosh(eta)
      enddo

c---------------------------------------------------------------------
c         call for jet finders
c---------------------------------------------------------------------
      i_opt = ems_jetpar(1).jetmethod
	  if (i_opt.ne.1) then
       write(*,*)
     1'EMS_ERJ -- Jet Finder option not coded yet...Exiting...'
       EMS_ERJ = STAFCV_BAD
	   return
	  end if
      njet = 0
      go to (10,20,30), i_opt
   10 call UA1(sum_down,
     1         ems_jetpar(1).cone_rad,ems_jetpar(1).et_seed,
     1         ems_jetpar(1).ej_min,ems_jetpar(1).et_min,
     1         ierror)
      go to 100
   20 call cdf
      go to 100
   30 call isa
      go to 100

c---------------------------------------------------------------------
  100 continue
      if(njet   .eq. 0) go to 999   ! exit
      if(ierror .eq. 1) then
       write(*,*)
     1'EMS_ERJ -- Found error condition from routine UA1...'
       EMS_ERJ = STAFCV_BAD
       go to 999                    ! exit
      end if
c---------------------------------------------------------------------

c----------------------------------------------------------------
c  Clusterization is over. Starting from here user can make 
c           a desirable selection of clusters.
c----------------------------------------------------------------
      if(ems_jetpar(1).jetweighting.eq.1)then
       weighted = .TRUE.
      else
       weighted = .FALSE.
      end if
      njet_bar = 0

      do i_jet=1,njet
         if(.not. weighted) then
           eta = p_jet(i_jet,1)
           phi = p_jet(i_jet,2)
         else
           eta = p_jet(i_jet,3)                    ! weighted eta
           phi = p_jet(i_jet,4)                    ! weighted phi
         endif

       if(abs(eta).lt.1.0.and.p_jet(i_jet,5)*cosh(eta).gt.
     &    ej_min)njet_bar = njet_bar + 1   ! # of barrel jets

      enddo                                        ! with E>Ej_min

c----------------------------------------------------------------
      if(njet_bar .eq. 0) then
       write(*,*)'EMS_ERJ -- no jets found...'
       go to 999
      end if
      if(njet_bar .gt. 100) then
       write(*,*)'EMS_ERJ -- Problem: more than 100 jets found!'
       go to 999
      end if
      write(*,*)'EMS_ERJ -- Found No. jets =',njet_bar
c----------------------------------------------------------------
      ijet_bar = 0
      do i_jet=1,njet
         i = nc + i_jet
         if(.not. weighted) then
           eta = p_jet(i_jet,1)
           phi = p_jet(i_jet,2)
         else
           eta = p_jet(i_jet,3)                           ! weighted eta
           phi = p_jet(i_jet,4)                           ! weighted phi
         endif
         theta = 2.*atan(exp(-eta))
         if(abs(eta).lt.1.0 .and. 
     *     p_jet(i_jet,5)*cosh(eta).gt.ej_min) then
          ijet_bar = ijet_bar + 1
          eta_b = int( (eta+1.0)/0.1 ) + 1
          if(phi .lt. 0.0) phi=twopi+phi           ! set phi=0...2pi
          if(phi .ge. 0.0) then
             phi_b = int(phi*180./pi/6.) + 1
          else
             phi_b= 60-int(abs(phi)*180./pi/6.)
          endif
          eta_grav = p_jet(i_jet,3)
          phi_grav = p_jet(i_jet,4)
          e_t_jet  = p_jet(i_jet,5)
          theta      = 2.* atan(exp(-eta     ))
          theta_grav = 2.* atan(exp(-eta_grav))
          eta_bin = eta_b
          phi_bin = phi_b
          if(ems_jetlist_h.nok .lt. ems_jetlist_h.maxlen) then
             ems_jetlist_h.nok = ems_jetlist_h.nok + 1
             l = ems_jetlist_h.nok
             ems_jetlist(l).id   = ijet_bar
             px = p_jet(i_jet,5)*cos(phi_grav)
             py = p_jet(i_jet,5)*sin(phi_grav)
             pz = p_jet(i_jet,5)*sinh(eta_grav)
             ems_jetlist(l).p_j(1) = px
             ems_jetlist(l).p_j(2) = py
             ems_jetlist(l).p_j(3) = pz
             ej = p_jet(i_jet,5)*cosh(eta_grav)
             ems_jetlist(l).e_j    = ej
             ems_jetlist(l).jet_m= sqrt(max(0.0, ej**2-px**2
     &                                      -py**2-pz**2))
             ems_jetlist(l).eta = eta
             ems_jetlist(l).phi = phi
             ems_jetlist(l).eta_w = eta_grav
             ems_jetlist(l).phi_w = phi_grav
             ems_jetlist(l).n_jet = njet_bar             !!! for a while   !!!
             ems_jetlist(l).n_cells = ns_cells(i_jet) 
             ems_jetlist(l).id_b = 0 
          else
           write(*,*)
     1      'EMS_ERJ -- WARNING!!! ems_jetlist is full - rows missed!'
           write(*,*)
     1      '        -- WARNING!!! increase .maxlen and redo...'
          endif
       endif
      enddo
c
      nmark=0
      marki=0
      markj=0
      do k=1,100
        imark(k)=0
      enddo
      do 40 i=1,njet_bar 
        do ik=1,100
          if(i.eq.imark(ik)) go to 40
        enddo 
        phil=ems_jetlist(i).phi_w
        do 50 j=1,njet_bar
          if(i.eq.j) go to 50
          phis=ems_jetlist(j).phi_w
          dphi=abs(phil-phis)*57.2958-180.
          if(abs(dphi).le.ems_jetpar(1).phi_dev) then
            nmark=nmark+1
            if(nmark.eq.1) then
              marki=i
              markj=j
            else
              marki=0
              markj=0
            endif
            imark(nmark)=j
          endif
 50     enddo
 40   enddo    
      if(marki.eq.0.and.markj.eq.0) go to 999
      ems_jetlist(marki).id_b=markj
      ems_jetlist(markj).id_b=marki

      EMS_ERJ=STAFCV_OK
999   RETURN
      END
