CC:>--------------------------------------------------------------------
CC: FILE:       dep_e_toadc.F
CC: HISTORY: Created Pavlinov Aleksei 02-aug-1998
CC: Corr. 13-aug for output from ems_interface2 by Akio
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
CC: DESCRIPTION: Physics Analysis Module FORTRAN 77 template.
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

      integer*4 key, ih, iha, ih1, adc, nphe, ierr
     +        , idh, idha
      real c1, c2, c3, radc, amu
     +        , hen
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

         write(6,*) ' From DE to ADC # control = ',control_toadc(1).mode
         write(6,1) ems_control(1).sampfrac 
     +  , ems_control(1).bemc_adcbits,control_toadc(1).energy
     +  , control_toadc(1).nphe_mip , control_toadc(1).dep_mip
     +  , c1, c2, c3
 1       format(
     + ' Sample Fraction coefficient                  ', f10.4/
     + ' Barrel Emc Tower ADC Bits                    ', i10/
     + ' Max energy in BEMC towers                    ', f10.2/
     + ' Mean Value of Number of PHE for MIP in BEMC  ', f10.2/
     + ' Mean Value of DE for MIP in BEMC             ', f10.6/
     + ' Coeff. transition from Deposit Energy to ADC ', f10.4/
     + ' Coeff. transition from Deposit Energy to PhE ', f10.4/
     + ' Coeff. transition from PhE            to ADC ', f10.4/)
      endif

      dep_e_toadc    = STAFCV_BAD       ! default is failure...
      adc_bemc_h.nok = 0
      if(hits_bemc_h.nok .le. 0) return 

      iha = 0  ! Number of entry to ADC array
      do ih=1,hits_bemc_h.nok 

        if(hits_bemc(ih).tower_dep .eq. 2) then
          iha = iha + 1
          adc_bemc(iha).det     = 1     ! BEMC
          adc_bemc(iha).eta_bin = hits_bemc(ih).eta_bin 
          adc_bemc(iha).phi_bin = hits_bemc(ih).phi_bin 
          adc_bemc(iha).energy  = hits_bemc(ih).e_sum
c ems_control(1).sampfrac

c          do ih1=1,hits_bemc_h.nok ! Preshower Exist?
c            if((hits_bemc(ih).eta_bin.eq.hits_bemc(ih1).eta_bin).and.
c     +         (hits_bemc(ih).phi_bin.eq.hits_bemc(ih1).phi_bin).and.
c     +         (hits_bemc(ih1).tower_dep.eq.1)) then 
c              adc_bemc(iha).energy = adc_bemc(iha).energy 
c     +        + hits_bemc(ih1).e_sum
c ems_control(1).sampfrac
c              goto 100
c            endif  
c          enddo
c 100      continue
        
        elseif(hits_bemc(ih).tower_dep .eq. 1) then
          do ih1=1,hits_bemc_h.nok ! Main tower exits
            if((hits_bemc(ih).eta_bin.eq.hits_bemc(ih1).eta_bin).and.
     +         (hits_bemc(ih).phi_bin.eq.hits_bemc(ih1).phi_bin).and.
     +         (hits_bemc(ih1).tower_dep.eq.2)) goto 200
          enddo
            
          iha = iha + 1
          adc_bemc(iha).det     = 1     ! BEMC
          adc_bemc(iha).eta_bin = hits_bemc(ih).eta_bin 
          adc_bemc(iha).phi_bin = hits_bemc(ih).phi_bin 
          adc_bemc(iha).energy  = hits_bemc(ih).e_sum
c ems_control(1).sampfrac
 200      continue
        else
          write(*,*) ' Bad Value of tower_dep ', hits_bemc(ih).tower_dep
     +             , ' hits ', ih
        endif  !  (hits_bemc(ih).tower_dep .eq. 1 or 2

        if(control_toadc(1).mode .eq. 0) then ! No ADC
          adc  = -1
        elseif(control_toadc(1).mode .eq. 1) then ! DE => ADC
          radc = hits_bemc(ih).e_sum * c1
          adc  = int (radc+0.5)    ! ??  
        elseif(control_toadc(1).mode .eq. 2) then ! DE => PHE => ADC
          amu  = hits_bemc(ih).e_sum * c2
          call rnpssn(amu, nphe, ierr)    !   Cernlib v136
          radc = float(nphe) * c3
          adc  = int (radc+0.5)    ! ??  
        else
          write(6,*) ' <W> Bad value control_toadc.mode '
        endif    ! control_toadc(1).mode .eq. 0,1,2
        adc=min(ems_control(1).bemc_adcbits,adc)
        adc=max(0,adc)
        adc_bemc(iha).adc = adc
            
      enddo                                 ! do ihit=1, emc_hit_h.nok

      if(iha.ge.1 .and. iha.le.adc_bemc_h.maxlen) then 
C: Successful completion of analysis module...
        adc_bemc_h.nok = iha
        dep_e_toadc = STAFCV_OK
      endif

      return
      end
 

