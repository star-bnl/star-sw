c:>------------------------------------------------------------------
C:FILE:         emc_erj.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       OGAWA, AKio, PSU/BNL
c:AUTHOR:       Originaly W.J. Llope, Rice University
c:AUTHOR:       TAS Version by B. Christie
C:BUGS:         None known.
C:HISTORY:      31jul96-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION EMC_ERJ(
     1       ems_control_h,       ems_control ,
     2        emc_energy_h,        emc_energy ,
     3        emc_jetpar_h,        emc_jetpar ,
     4       ems_jetlist_h,       ems_jetlist ) 
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMC_ERJ
C:DESCRIPTION:  Jet-finder, applies UA1-like algorithm from LUND JETSET.
C:ARGUMENTS:    ems_control emc_energy emc_jetpar
C:ARGUMENTS:    ems_jetlist
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE
#include "emc_erj.inc"
      real pi,twopi
      common/erjkeep/ pi,twopi
      integer ncell_tot
      real ej_min
      save ncell_tot
      integer NMAX, JMAX
      parameter(NMAX=10000, JMAX=100)
      integer ncell, njet, ierror, ncellj(JMAX)
      real etc(NMAX),etac(NMAX),phic(NMAX)
      real etj(JMAX),etaj(JMAX,2),phij(JMAX,2)
      integer i,j,nout,i_jet
      integer ncell_bemc,ncell_eemc,ncell_chrg
      integer nmark,marki,markj,k,ik,imark(JMAX)
      real eta,phi,e_j,px,py,pz
      real phil,phis,dphi,r_int
c---------------------------------------------------------------------
C       Initialize Jet parameters
c---------------------------------------------------------------------
      if(emc_jetpar(1).init.eq.1)then  ! Only do this once
         emc_jetpar(1).init = 0
         write(*,*)'EMC_ERJ -- The jet finding parameters are:'
         write(*,*)
     1        'EMC_ERJ -- Jet Finding Method                      =',
     1        EMC_jetpar(1).jetmethod
         write(*,*)
     1        'EMC_ERJ -- Enable eta,phi Weighting?               =',
     1        EMC_jetpar(1).jetweighting
         write(*,*)
     1        'EMC_ERJ -- Cone Radius                             =',
     1        EMC_jetpar(1).cone_rad
         write(*,*)
     1        'EMC_ERJ -- Min. energy for post-algorithm selection=',
     1        EMC_jetpar(1).ej_min
         write(*,*)
     1        'EMC_ERJ -- Min. seed tower energy                  =',
     1        EMC_jetpar(1).et_seed
         write(*,*)
     1        'EMC_ERJ -- Min. tower energy to be included        =',
     1        EMC_jetpar(1).et_min
         write(*,*)
     1        'EMC_ERJ -- Maximum phi deviation for back-to-back  =',
     1        EMC_jetpar(1).phi_dev
         write(*,*)
     1        'EMC_ERJ -- Background subtraction mode off=0/on=1  =',
     1        EMC_jetpar(1).mode_bg
         ncell_bemc=ems_control(1).bemc_eta_nbin
     +             *ems_control(1).bemc_phi_nbin
         ncell_eemc=ems_control(1).eemc_eta_nbin
     +             *ems_control(1).eemc_phi_nbin
         ncell_chrg=ems_control(1).chrg_eta_nbin
     +             *ems_control(1).chrg_phi_nbin
         ncell_tot =ncell_bemc+ncell_eemc+ncell_chrg
         write(*,*)
     1        'EMC_ERJ -- # of cell in BEMC/EEMC/track/Total      =',
     1        ncell_bemc,ncell_eemc,ncell_chrg,ncell_tot
         pi    = ACOS(-1.0)
         twopi = 2.0*pi
      endif
c---------------------------------------------------------------------
C     Initialize nok counter and variable arrays
c---------------------------------------------------------------------
      EMC_erj = STAFCV_OK
      ems_jetlist_h.nok = 0
c---------------------------------------------------------------------
C      Load Energy Grid for Jet Finder
c---------------------------------------------------------------------
      ncell=EMC_energy_h.nok
      if(ncell .gt. NMAX) then
         write(*,*) 'EMC_ERJ: Assign more memory for et_tot'
         EMC_erj = STAFCV_BAD
         return
      endif
      do i=1,ncell
        etc(i)  = emc_energy(i).energy/cosh(EMC_energy(i).eta)
        etac(i) = emc_energy(i).eta
        phic(i) = emc_energy(i).phi
      enddo
c---------------------------------------------------------------------
c         call for jet finders
c---------------------------------------------------------------------
      njet = 0
      i = EMC_jetpar(1).jetmethod
      if(i.eq.1) then
         call jet_finder_UA1(ncell, ncell_tot, etc, etac, phic,
     +        njet, etj, etaj, phij, ncellj,
     +        EMC_jetpar(1).cone_rad,EMC_jetpar(1).et_seed,
     +        EMC_jetpar(1).ej_min,EMC_jetpar(1).et_min,
     +        EMC_jetpar(1).min_cone_move,EMC_jetpar(1).max_cone_move,
     +        EMC_jetpar(1).mode_bg, EMC_jetpar(1).prec_bg, ierror)
      elseif(i.eq.2) then
         call jet_finder_cdf
      elseif(i.eq.3) then
         call jet_finder_isa
      else
         write(*,*)
     1     'ERJ -- Jet Finder option not coded yet...Exiting...'
         EMC_ERJ = STAFCV_BAD
         return
      endif
      if(njet .eq. 0) then
         write(*,*)'EMC_ERJ -- no jets found at all!!!'
         return
      endif
      if(ierror .eq. 1) then
         write(*,*)
     1     'EMC_ERJ -- Found error condition from routine UA1...'
         EMC_ERJ = STAFCV_BAD
         return
      end if
      write(*,*)'EMC_ERJ -- Found No. jets before cut =',njet
c----------------------------------------------------------------
c  Clusterization is over. Starting from here user can make 
c           a desirable selection of clusters.
c----------------------------------------------------------------      
      nout = 0
      do i_jet=1,njet
         if(EMC_jetpar(1).jetweighting.eq.0) then
           eta = etaj(i_jet,1)
           phi = phij(i_jet,1)
         else
           eta = etaj(i_jet,2)                    ! weighted eta
           phi = phij(i_jet,2)                    ! weighted phi
         endif
         e_j=etj(i_jet)*cosh(eta)
         if(eta .ge. emc_jetpar(1).eta_min .and.
     +      eta .le. emc_jetpar(1).eta_max .and.
     +      e_j .gt. emc_jetpar(1).ej_min) then
            if(nout .ge. ems_jetlist_h.maxlen) then
               write(*,*) 'EMC_ERJ -- WARNING!!! ems_jetlist is full'
               write(*,*) 'EMC_ERJ -- increase .maxlen and redo...'
               EMC_ERJ = STAFCV_BAD
               return
            endif
            nout=nout+1
            ems_jetlist(nout).id     = nout
            ems_jetlist(nout).eta    = etaj(i_jet,1)
            ems_jetlist(nout).phi    = phij(i_jet,1)
            ems_jetlist(nout).eta_w  = etaj(i_jet,2)
            ems_jetlist(nout).phi_W  = phij(i_jet,2)           
            ems_jetlist(nout).e_j    = e_j
            ems_jetlist(nout).n_cells= ncellj(i_jet) 
            px = etj(i_jet)*cos(phi)
            py = etj(i_jet)*sin(phi)
            pz = etj(i_jet)*sinh(eta)
            ems_jetlist(nout).p_j(1) = px
            ems_jetlist(nout).p_j(2) = py
            ems_jetlist(nout).p_j(3) = pz
            ems_jetlist(nout).jet_m  = 
     +             sqrt(max(0.0, e_j**2-px**2-py**2-pz**2))
            ems_jetlist(nout).n_jet  = njet           !!! for a while   !!!
            ems_jetlist(nout).id_b   = 0 
         endif
         ems_jetlist_h.nok=nout
      enddo

      write(*,*)'EMC_ERJ -- Found No. jets after cut =',nout
      if(nout .eq. 0) then
         write(*,*)'EMC_ERJ -- no jets found after ej_min/eta cuts'
         return
      end if

c*-back-to-back criteria
      nmark=0
      marki=0
      markj=0
      do k=1,100
        imark(k)=0
      enddo
      do 40 i=1,nout
        do ik=1,100
          if(i.eq.imark(ik)) go to 40
        enddo 
        phil=ems_jetlist(i).phi_w
        do 50 j=1,nout
          if(i.eq.j) go to 50
          phis=ems_jetlist(j).phi_w          
          dphi=phil-phis
          do while(dphi.ge.twopi)
             dphi=dphi-twopi
          enddo
          do while(dphi.lt.0.0)
             dphi=dphi+twopi
          enddo
          dphi=dphi*57.2958-180.
          if(abs(dphi).le.emc_jetpar(1).phi_dev) then
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

      EMC_ERJ=STAFCV_OK
999   RETURN
      END
