      INTEGER*4 FUNCTION EMS_LVL0(
     1       ems_control_h,       ems_control ,
     2     ems_hits_bemc_h,     ems_hits_bemc ,
     3    ems_lvl0_thres_h,    ems_lvl0_thres ,
     4     ems_lvl0_adcs_h,     ems_lvl0_adcs ,
     5     ems_lvl0_bits_h,     ems_lvl0_bits ) 
c:>------------------------------------------------------------------
C:FILE:         ems_lvl0.F
C:DESCRIPTION:  EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       W.J. Llope, Rice University (TAS Version by B. Christie)
C:BUGS:         None known.
C:HISTORY:      31jul96-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMS_LVL0
C:DESCRIPTION:  calculates trigger ADCs and bit patterns from EMC...
C:              tower information. NOTE it uses 0.1 x 0.1 towers,
C:              whether or not you've selected 0.05 x 0.005 or 
C:              0.1 x 0.1 towers via the ems_control table... 
C:ARGUMENTS:    ems_control ems_hits_bemc ems_lvl0_thres
C:ARGUMENTS:    ems_lvl0_adcs ems_lvl0_bits
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
      IMPLICIT NONE
C
#include "ems_lvl0.inc"
C
      LOGICAL first_call/.TRUE./
      INTEGER i, j, k, ihit
      INTEGER i_eta, i_phi, i_dep
      INTEGER test, nok
      INTEGER ierr,itwr
      INTEGER J_UP
      INTEGER phi_gross,eta_gross,iso_bin
      INTEGER lum_cnt
      INTEGER four_t_id,icnt
      INTEGER one_tow_cnt(3)
      INTEGER four_tow_cnt(3)
      INTEGER sixt_tow_cnt(3)
      REAL eb_sum(60,20)      ! Total Energy
      REAL emc_tot            ! Total sampled energy in Barrel EMC
      REAL iso_adc(20)        ! Energy in a 1 (eta)x 1(phi) patch
      REAL one_tow_check(3)   ! Converted thres. for single towers.
      REAL four_tow_check(3)  ! Converted thres. for 4 tow. clusters.
      REAL sixt_tow_check(3)  ! Converted thres. for 16 tow. clusters.
      REAL four_tow_sum(300)  ! Energy in fixed four tower clusters
      REAL sixt_tow_sum       ! Energy in 16 tower cluster
      REAL one_tow_def(3)     ! Default values for 1 tower thresholds
      REAL four_tow_def(3)    ! Default values for 4 tower thresholds
      REAL sixt_tow_def(3)    ! Default values for 16 tower thresholds
      REAL glob_def(3)        ! Default values for global E thresholds
      REAL lum_def(4)         ! Deafault values for lum. thresholds
      REAL one_thres_old      ! Single Tower trig. thres. from last call of mod.
      REAL cal                ! Sampling frac. of EMC for EM energy
      REAL thres_chk          ! High tower thres./EMC samp. frac. for EM E.

      INTEGER eta_pm,phi_bin,eta_bin,out_num,num_in
      INTEGER pbin,ebin
      INTEGER int_yn,pb_num
      INTEGER pbin_lo,pbin_hi,ebin_lo,ebin_hi
      INTEGER ebin_four,pbin_four
      INTEGER iso_ebin(20),iso_pbin(20)
      INTEGER etot_max  ! Maximum sampled energy expected in barrel
             ! N.B. This value should be set depending on the species
             !in RHIC (pp, pA, AA). In effect, this value sets the
             !energy scale for the ADC.
      INTEGER iso_max   ! Max. energy expected in isospin patch.

      LOGICAL sum_down/.FALSE./

C---- Data Statements... 
      DATA cal/16.00/   ! Conversion from Neutral E in tower to Neutral Energy incident on tower
      DATA etot_max/4/  ! Max. sampled energy expected in barrel.   
      DATA iso_max/2/   ! Max. sampled energy expected in iso patch.

C       N.B. Thresholds are input as equivalent, incident EM energy.
C       These values are divided by the sampling factor given above
C       (cal~16) before being compared to the tower energies.

C---- Executable Statements
      EMS_LVL0=STAFCV_BAD		! default is failure...

c---- first call code...
      if(first_call)then
        first_call	= .FALSE.
c---- print thresholds that will be used...
        write(*,*)'EMS_LVL0 -- Thresholds that will be used...'
        write(*,*)
     1'EMS_LVL0 -- Single Tower thresholds are (GeV, EM equiv.): '
        write(*,*)ems_lvl0_thres(1).one_tow_thres(1)
        write(*,*)ems_lvl0_thres(1).one_tow_thres(2)
        write(*,*)ems_lvl0_thres(1).one_tow_thres(3)
        write(*,*)
     1'EMS_LVL0 -- Four Tower thresholds are (GeV, EM equiv.): '
        write(*,*)ems_lvl0_thres(1).four_tow_thres(1)
        write(*,*)ems_lvl0_thres(1).four_tow_thres(2)
        write(*,*)ems_lvl0_thres(1).four_tow_thres(3)
        write(*,*)
     1'EMS_LVL0 -- Sixteen Tower thresholds are (GeV, EM equiv.): '
        write(*,*)ems_lvl0_thres(1).sixteen_tow_thres(1)
        write(*,*)ems_lvl0_thres(1).sixteen_tow_thres(2)
        write(*,*)ems_lvl0_thres(1).sixteen_tow_thres(3)
        write(*,*)
     1'EMS_LVL0 -- Barrel Energy thresholds are (GeV, EM equiv.): '
        write(*,*)ems_lvl0_thres(1).glob_thres(1)
        write(*,*)ems_lvl0_thres(1).glob_thres(2)
        write(*,*)ems_lvl0_thres(1).glob_thres(3)
        write(*,*)
     1'EMS_LVL0 -- No. of single towers for Luminosity: '
        write(*,*)ems_lvl0_thres(1).lum_thres(1)
        write(*,*)ems_lvl0_thres(1).lum_thres(2)
        write(*,*)ems_lvl0_thres(1).lum_thres(3)
        write(*,*)
     1'EMS_LVL0 -- Energy threshold for tower counting:'
        write(*,*)ems_lvl0_thres(1).lum_thres(4)
        write(*,'(1x,a49,i4,1x,a24)')
     1    'EMS_LVL0 -- isospin patch ADC=256 corresponds to ',
     1     iso_max,'GeV of sampled energy...'
        write(*,'(1x,a46,i4,1x,a24)')
     1    'EMS_LVL0 -- global sum ADC=256 corresponds to ',
     1     etot_max,'GeV of sampled energy...'
      endif		! end first call code...

C---- initialize variables...
      ems_lvl0_adcs_h.nok = 0
      ems_lvl0_bits_h.nok = 0
      out_num = 0
      num_in = 0
      emc_tot = 0.
      lum_cnt = 0
      do j = 1,20
       do i = 1,60
        eb_sum(i,j) = 0.
       end do
      end do
      do i = 1,20
       iso_adc(i) = 0.0
      end do
      do i = 1,3
       one_tow_cnt(i)  = 0
       four_tow_cnt(i) = 0
       sixt_tow_cnt(i) = 0
      end do
      sum_down = .FALSE.
      if(ems_control(1).bemc_Ntowers.eq.1200)
     1   sum_down = .TRUE.

c---- copy the tower/depth energies into a local array...
      do ihit  = 1,ems_hits_bemc_h.nok
        i_dep  = ems_hits_bemc(ihit).tower_dep
        if(i_dep.eq.0)then          ! location of sum over depths from ems_interface...
         i_eta  = ems_hits_bemc(ihit).eta_bin
         i_phi  = ems_hits_bemc(ihit).phi_bin
         if(.not.sum_down)then  	! check if summing to 0.1x0.1 is needed...
          i_eta = (i_eta-1)/2 + 1
          i_phi = (i_phi-1)/2 + 1
         end if
         eb_sum(i_phi,i_eta) = ems_hits_bemc(ihit).e_sum
         emc_tot             = emc_tot + ems_hits_bemc(ihit).e_sum   
        end if
      enddo                       ! End loop over input EMC towers

C       Load the ADC values for the ISOSPIN patches. These patches
C       are 1 unit in eta by one unit in phi. This leads to 12
C       patches total for the EMC barrel, and 6 patches for the
C       EMC endcap.

      do eta_gross = 1,2         ! Loop over gross (1 unit) eta patches
         ebin_lo   = (10*eta_gross) - 9
         ebin_hi   =  10*eta_gross
         do phi_gross = 1,6      ! Loop over gross (1 unit) phi patches
            pbin_lo = (10*phi_gross) - 9
            pbin_hi = (10*phi_gross)
            iso_bin = eta_gross*phi_gross 
     1              + (eta_gross-1)*6              ! Patch #. Range 1->12
            do eta_bin = ebin_lo,ebin_hi
               do phi_bin = pbin_lo,pbin_hi
                  iso_adc(iso_bin) = iso_adc(iso_bin)
     1                             + eb_sum(phi_bin,eta_bin)
                  if(eta_gross.eq.1)then
                        iso_ebin(iso_bin) = -1
                  else
                        iso_ebin(iso_bin) = 1
                  endif
                  iso_pbin(iso_bin) = phi_gross
               enddo    ! End loop over phi_bin
            enddo       ! End loop over phi_bin
         enddo          ! End loop over phi_gross
      enddo             ! End loop over eta_gross

c       Check to see how many (if any) single Towers are over the various
c       single Tower trigger thresholds. There is an implicit assumption 
c       that the particles that hit a tower, causing it to have an
c       Energy value greater than the threshold, are electromagnetic.
c       As such, the tower energy is compared to the threshold
c       divided by the sampling factor of the EMC for incident EM
c       energy (i.e. threshold/16.0).

        one_tow_check(1) = ems_lvl0_thres(1).one_tow_thres(1)/cal
        one_tow_check(2) = ems_lvl0_thres(1).one_tow_thres(2)/cal
        one_tow_check(3) = ems_lvl0_thres(1).one_tow_thres(3)/cal

        do pbin = 1,60          ! Loop over the phi_bins
           do ebin = 1,20       ! Loop over the eta_bins
             if(eb_sum(pbin,ebin).ge.one_tow_check(1))then
                one_tow_cnt(1) = one_tow_cnt(1) + 1
             endif
             if(eb_sum(pbin,ebin).ge.one_tow_check(2))then
                one_tow_cnt(2) = one_tow_cnt(2) + 1
             endif
             if(eb_sum(pbin,ebin).ge.one_tow_check(3))then
                one_tow_cnt(3) = one_tow_cnt(3) + 1
             endif

C---- Count how many towers are above the Energy thres. for Lum. monitoring
             if(eb_sum(pbin,ebin).ge.
     1          ems_lvl0_thres(1).lum_thres(4)/cal)then
               lum_cnt = lum_cnt + 1
             endif
           enddo        ! End loop over ebin
        enddo           ! End loop over pbin

c       Check to see how many (if any) 4 Tower sums are over the various
c       four Tower trigger thresholds. There is an implicit assumption 
c       that the particles that hit a 4 tower cluster, causing it to have 
c       an Energy value greater than the threshold, are electromagnetic.
c       As such, the 4 tower energy is compared to the threshold
c       divided by the sampling factor of the EMC for incident EM
c       energy (i.e. threshold/16.0). N.B. Only one specific set of 4
c       tower sums are calculated.

        four_tow_check(1) = ems_lvl0_thres(1).four_tow_thres(1)/cal
        four_tow_check(2) = ems_lvl0_thres(1).four_tow_thres(2)/cal
        four_tow_check(3) = ems_lvl0_thres(1).four_tow_thres(3)/cal
        four_t_id = 0
        do pbin = 1,59,2          ! Loop over the phi_bins, by 2's
           do ebin = 1,19,2       ! Loop over the eta_bins, by 2's
             four_t_id = four_t_id + 1      ! ID for 4 tow. clust.
             four_tow_sum(four_t_id) = 0.0  ! Initialize to zero
             do pbin_four = pbin,pbin+1
               do ebin_four = ebin,ebin+1
                 four_tow_sum(four_t_id) = four_tow_sum(four_t_id)
     +           + eb_sum(pbin_four,ebin_four)
               enddo
             enddo
             if(four_tow_sum(four_t_id).ge.four_tow_check(1))then
                four_tow_cnt(1) = four_tow_cnt(1) + 1
             endif
             if(four_tow_sum(four_t_id).ge.four_tow_check(2))then
                four_tow_cnt(2) = four_tow_cnt(2) + 1
             endif
             if(four_tow_sum(four_t_id).ge.four_tow_check(3))then
                four_tow_cnt(3) = four_tow_cnt(3) + 1
             endif
           enddo        ! End loop over ebin
        enddo           ! End loop over pbin

c       Check to see how many (if any) 16 Tower sums are over the various
c       sixteen Tower trigger thresholds. There is an implicit assumption 
c       that the particles that hit a 16 tower cluster, causing it to have 
c       an Energy value greater than the threshold, are electromagnetic.
c       As such, the 16 tower energy is compared to the threshold
c       divided by the sampling factor of the EMC for incident EM
c       energy (i.e. threshold/16.0). N.B. All possible, contiguous,
c       square (i.e. 4x4), 16 tower sums are calculated.

        sixt_tow_check(1) = ems_lvl0_thres(1).sixteen_tow_thres(1)/cal
        sixt_tow_check(2) = ems_lvl0_thres(1).sixteen_tow_thres(2)/cal
        sixt_tow_check(3) = ems_lvl0_thres(1).sixteen_tow_thres(3)/cal
        do icnt = 1,291,10               ! Loop over 4 tower sums, by 10's
           do four_t_id = icnt,icnt+8,2  ! Loop over 4 tower sums, by 2's
              sixt_tow_sum = 0.0
              if(icnt.ne.291)then
                sixt_tow_sum = four_tow_sum(four_t_id) + 
     +                         four_tow_sum(four_t_id+1) +
     +                         four_tow_sum(four_t_id+10) + 
     +                         four_tow_sum(four_t_id+11)
              else      ! Take care of seam on cyl. where one starts.
                sixt_tow_sum = four_tow_sum(four_t_id) + 
     +                         four_tow_sum(four_t_id+1) +
     +                         four_tow_sum(four_t_id-290) + 
     +                         four_tow_sum(four_t_id-289)
              endif
             if(sixt_tow_sum.ge.sixt_tow_check(1))then
                sixt_tow_cnt(1) = sixt_tow_cnt(1) + 1
             endif
             if(sixt_tow_sum.ge.sixt_tow_check(2))then
                sixt_tow_cnt(2) = sixt_tow_cnt(2) + 1
             endif
             if(sixt_tow_sum.ge.sixt_tow_check(3))then
                sixt_tow_cnt(3) = sixt_tow_cnt(3) + 1
             endif
           enddo        ! End loop over four_t_id
        enddo           ! End loop over icnt    

C     Fill Output Table with trigger Bits

      ems_lvl0_bits_h.nok = 1       ! ALWAYS put out one group

C     Put out Global ADC Value. In converting from the sampled energy
C     in GeV to a digital ADC value, I assume that it is an 8 bit ADC
C     (max cnt. = 256), and that the top of the scale is set to be
C     equivalent to a sampled energy given by etot_max. N.B. This
C     maximum etot_max should be adjusted depending on what system is
C     being run (pp, pA, or AA)
C     ---------------------------------------------------------------

      ems_lvl0_bits(1).glob_adc  = (emc_tot/etot_max)*256

C     Output Single Tower Trigger bits
C     ---------------------------------

      if(one_tow_cnt(1).gt.0)then
          ems_lvl0_bits(1).one_tow(1) = 1
      else
          ems_lvl0_bits(1).one_tow(1) = 0
      endif

      if(one_tow_cnt(2).gt.0)then
          ems_lvl0_bits(1).one_tow(2) = 1
      else
          ems_lvl0_bits(1).one_tow(2) = 0
      endif

      if(one_tow_cnt(3).gt.0)then
          ems_lvl0_bits(1).one_tow(3) = 1
      else
          ems_lvl0_bits(1).one_tow(3) = 0
      endif

C     Output Four Tower Trigger bits
C     ------------------------------

      if(four_tow_cnt(1).gt.0)then
          ems_lvl0_bits(1).four_tow(1) = 1
      else
          ems_lvl0_bits(1).four_tow(1) = 0
      endif

      if(four_tow_cnt(2).gt.0)then
          ems_lvl0_bits(1).four_tow(2) = 1
      else
          ems_lvl0_bits(1).four_tow(2) = 0
      endif

      if(four_tow_cnt(3).gt.0)then
          ems_lvl0_bits(1).four_tow(3) = 1
      else
          ems_lvl0_bits(1).four_tow(3) = 0
      endif

C     Output 16 Tower Trigger bits
C     ------------------------------

      if(sixt_tow_cnt(1).gt.0)then
          ems_lvl0_bits(1).sixteen_tow(1) = 1
      else
          ems_lvl0_bits(1).sixteen_tow(1) = 0
      endif

      if(sixt_tow_cnt(2).gt.0)then
          ems_lvl0_bits(1).sixteen_tow(2) = 1
      else
          ems_lvl0_bits(1).sixteen_tow(2) = 0
      endif

      if(sixt_tow_cnt(3).gt.0)then
          ems_lvl0_bits(1).sixteen_tow(3) = 1
      else
          ems_lvl0_bits(1).sixteen_tow(3) = 0
      endif

C     Output Global Energy Trigger Bits
C     ---------------------------------

      if(emc_tot.gt.ems_lvl0_thres(1).glob_thres(1)/cal)then
          ems_lvl0_bits(1).glob_bits(1) = 1
      else
          ems_lvl0_bits(1).glob_bits(1) = 0
      endif         
      if(emc_tot.gt.ems_lvl0_thres(1).glob_thres(2)/cal)then
          ems_lvl0_bits(1).glob_bits(2) = 1
      else
          ems_lvl0_bits(1).glob_bits(2) = 0
      endif
      if(emc_tot.gt.ems_lvl0_thres(1).glob_thres(3)/cal)then
          ems_lvl0_bits(1).glob_bits(3) = 1
      else
          ems_lvl0_bits(1).glob_bits(3) = 0
      endif

C     Output Luminosity Monitoring Trigger Bits
C     -----------------------------------------

      if(lum_cnt.gt.ems_lvl0_thres(1).lum_thres(1))then
          ems_lvl0_bits(1).lum_bits(1) = 1        
      else
          ems_lvl0_bits(1).lum_bits(1) = 0        
      endif
      if(lum_cnt.gt.ems_lvl0_thres(1).lum_thres(2))then
          ems_lvl0_bits(1).lum_bits(2) = 1  
      else
          ems_lvl0_bits(1).lum_bits(2) = 0  
      endif
      if(lum_cnt.gt.ems_lvl0_thres(1).lum_thres(3))then
          ems_lvl0_bits(1).lum_bits(3) = 1  
      else
          ems_lvl0_bits(1).lum_bits(3) = 0  
      endif

C    Fill ISOSPIN output table. The conversion from the sampled
C    energy in an isospin patch to an ADC value assumes an 8 bit ADC
C    (max. cnt. = 256). The ADC to energy scale is set by the value of
C    iso_max. This value of iso_max should be adjusted depending on
C    what system is being studied (pp, pA, or AA).

      out_num = 0
      do iso_bin = 1,20      ! Loop over isospin patches
       if(out_num.lt.ems_lvl0_adcs_h.maxlen) then
        if(iso_adc(iso_bin).gt.0)then    ! Energy in patch
         out_num = out_num + 1
         ems_lvl0_adcs(out_num).eta_bin = iso_ebin(iso_bin)
         ems_lvl0_adcs(out_num).phi_bin = iso_pbin(iso_bin)
         ems_lvl0_adcs(out_num).iso_adc = 
     &                        (iso_adc(iso_bin)/iso_max)*256
        endif    ! Energy in patch
       else
        write(*,*)
     1   'EMS_LVL0 -- WARNING!!! ems_lvl0_adcs is full - rows missed!'
        write(*,*)
     1   '         -- WARNING!!! increase .maxlen and redo...'
       endif            ! Space available in output
      enddo             ! Loop over isospin patches
      ems_lvl0_adcs_h.nok = out_num

998   EMS_LVL0=STAFCV_OK
999   RETURN
      END
