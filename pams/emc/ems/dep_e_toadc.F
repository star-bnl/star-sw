CC:>--------------------------------------------------------------------
CC: FILE:       dep_e_toadc.F
CC: HISTORY: Created Pavlinov Aleksei 02-aug-1998
CC: Corr. 13-aug for output from ems_interface2 by Akio
CC: Corr. 22-sep => for dep=2 is dep.energy for whole tower 
CC: Corr. 23-sep => Added the shake of pedestals
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION DEP_E_TOADC(
     1       ems_control_h,       ems_control ,
     2     control_toadc_h,     control_toadc ,
     3         hits_bemc_h,         hits_bemc ,
     4          adc_bemc_h,          adc_bemc ) 
      IMPLICIT NONE
#include "dep_e_toadc.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    DEP_E_TOADC
CC: DESCRIPTION: Physics Analysis Module FORTRAN 77 for transition 
CC               from deposit energy to ADC.
CC: AUTHOR:     PAI - Pavlinov Aleksei, IHEP, Protvino
CC: ARGUMENTS:
CC:          IN:
CC:      ems_control     - control option for EMS package
CC:    ems_control_h     - Header Structure for ems_control
CC:    control_toadc     - control option for transition from DE to ADC
CC:  control_toadc_h     - Header Structure for control_toadc
CC:        hits_bemc     - Deposit Energy in BEMC
CC:      hits_bemc_h     - Header Structure for hits_bemc
CC:       INOUT:
CC:         OUT:
CC:         adc_bemc     - ADC for BEMC
CC:       adc_bemc_h     - Header Structure for adc_bemc
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

      integer*4 key, ih, iha, adc, nphe, ierr, ibad, mod, min, max
      real c1, c2, c3, radc, amu, rped(1)
      data key/0/

      if(key.eq.0) then ! Calculated value of coefficients
         key = 1
         c1  = (ems_control(1).sampfrac * ems_control(1).bemc_adcbits) 
     +       / control_toadc(1).energy
         c2  =  control_toadc(1).nphe_mip / control_toadc(1).dep_mip
         c3  = c1 / c2
         control_toadc(1).c1 = c1
         control_toadc(1).c2 = c2
         control_toadc(1).c3 = c3

         write(6,1) control_toadc(1).mode
         write(6,2) ems_control(1).sampfrac 
     +  , ems_control(1).bemc_adcbits,control_toadc(1).energy
     +  , control_toadc(1).nphe_mip , control_toadc(1).dep_mip
     +  , control_toadc(1).rms_ped
     +  , c1, c2, c3
      endif

      dep_e_toadc    = STAFCV_BAD       ! default is failure...
      adc_bemc_h.nok = 0
      if(hits_bemc_h.nok .le. 0) then
        write(6,*) ' <I> No hits in BEMC # hits = ', hits_bemc_h.nok 
        return 
      endif

      iha  = 0  ! Number of entry to ADC array
      ibad = 0  ! Number of bad hits

      do ih=1,hits_bemc_h.nok 
        if    (hits_bemc(ih).tower_dep.eq.2 .and. 
     +         hits_bemc(ih).e_sum.gt.0.0) then

          adc = 0
          if(control_toadc(1).mode .eq. 0) then ! No ADC
            adc  = -1

          elseif(control_toadc(1).mode .eq. 1) then ! DE => ADC
            radc = hits_bemc(ih).e_sum * control_toadc(1).c1
            adc  = int (radc+0.5)    ! Nearest positive integer number  

          elseif(control_toadc(1).mode .eq. 2) then ! DE => PHE => ADC
            amu  = hits_bemc(ih).e_sum * control_toadc(1).c2
            if(amu.gt.0.0) then
              call poissn(amu, nphe, ierr)    !   See mathlib
              radc = float(nphe) * control_toadc(1).c3

c   Shake of pedestal
              if(control_toadc(1).rms_ped .gt. 0.0) then
                call rnorml(rped,1)   ! Normal Distribution (V120 in Cernlib)
cc                call hf1(1001,rped(1), 1.)  ! For testing only => all work
                radc  = radc + rped(1) * control_toadc(1).rms_ped 
              endif

              adc     = int (radc+0.5)    ! Nearest positive integer number  

            else
              adc  = 0
            endif   ! if(amu.gt.0.0)

          else
            write(6,*) ' <W> Bad value control_toadc(1).mode='
     +                                ,control_toadc(1).mode
          endif    ! control_toadc(1).mode .eq. 0,1,2

          adc = min(ems_control(1).bemc_adcbits,adc)
          adc = max(0,adc)
            
          if(adc .gt. 0) then   ! Zero Suppression
            iha = iha + 1
            adc_bemc(iha).det     = 1     ! BEMC
            adc_bemc(iha).eta_bin = hits_bemc(ih).eta_bin 
            adc_bemc(iha).phi_bin = hits_bemc(ih).phi_bin 
            adc_bemc(iha).energy  = hits_bemc(ih).e_sum
            adc_bemc(iha).adc     = adc
            if(adc_bemc(iha).adc .ne. adc) then
              write(6,*) 'adc_bemc(iha).adc ',adc_bemc(iha).adc,' adc ', adc 
            endif
          endif

        elseif(hits_bemc(ih).tower_dep .eq. 1) then  ! Preshower
          continue
        else
          ibad = ibad + 1
          if(ibad.le.10 .or. mod(ibad-1,10).eq.0) then 
            write(*,*) ' <E> Bad Value of tower_dep ',hits_bemc(ih).tower_dep
     +             , ' hits ', ih, ' ibad ', ibad
          endif
        endif  !  (hits_bemc(ih).tower_dep .eq. 2
      enddo   !  do ih=1,hits_bemc_h.nok  

      if(iha .eq. 0) return    ! No hits 

      if(iha.ge.1 .and. iha.le.adc_bemc_h.maxlen) then 
C: Successful completion of analysis module...
        adc_bemc_h.nok = iha
        dep_e_toadc = STAFCV_OK

      elseif(iha .gt. adc_bemc_h.maxlen) then
        write(6,3) adc_bemc_h.maxlen , iha
      endif

      return
 1    format(' <I> Subroutine dep_e_toadc => Ver. 1.10',
     +       ' date => 30-sep-1998 # mode =', i2)
 2    format(
     + ' Sample Fraction coefficient                  ', f10.4/
     + ' Barrel Emc Tower ADC Bits                    ', i10/
     + ' Max energy in BEMC towers                    ', f10.2/
     + ' Mean Value of Number of PHE for MIP in BEMC  ', f10.2/
     + ' Mean Value of DE for MIP in BEMC             ', f10.6/
     + ' RMS of pedestals                             ', f10.2/
     + ' Coeff. transition from Deposit Energy to ADC ', f10.4/
     + ' Coeff. transition from Deposit Energy to PhE ', f10.4/
     + ' Coeff. transition from PhE            to ADC ', f10.4/)

 3    format(' <W> Change size of adc_bemc structure from ',i5
     + ,' to ',i6) 
      end
