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
     3          hits_emc_h,          hits_emc ,
     4          adc_emc1_h,          adc_emc1 , 
     5          adc_emc2_h,          adc_emc2 ) 
      IMPLICIT NONE
#include "emc_def.h"
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
CC:        hits_emc      - Deposit Energy in EMC
CC:      hits_emc_h      - Header Structure for hits_emc
CC:       INOUT:
CC:         OUT:
CC:         adc_emc     - ADC for EMC
CC:       adc_emc_h     - Header Structure for adc_emc
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

      integer det, i, mode
      integer*4 key, ih, o1, o2, adc, nphe, ierr, mod, min, max
      real c1(MAXDET), c2(MAXDET), c3(MAXDET), radc, amu
      data key/0/

      dep_e_toadc    = STAFCV_BAD       ! default is failure...

      if(key.eq.0) then ! Calculated value of coefficients
         key = 1
         write(6,1)
         do i=1,MAXDET
            write(6,2) i,control_toadc(1).mode(i)
            if(control_toadc(1).mode(i).eq.0) then
               write(6,*) '  No conversion will take place'
            elseif(control_toadc(1).energy(i).le.0.0 .or.
     +         control_toadc(1).dep_mip(i).le.0.0 .or.
     +         control_toadc(1).nphe_mip(i).le.0.0 .or.
     +         ems_control(1).sampfrac(i).le.0.0 .or.
     +         ems_control(1).adcbits(i).le.0) then
               write(6,*)'  Not enough informations for det=',i   
               write(6,*)'  mode is forced to be 0 (no conversion)!!!'   
               control_toadc(1).mode(i)=0
            else
               c1(i) = ( ems_control(1).sampfrac(i) 
     +              * ems_control(1).adcbits(i) ) 
     +              / control_toadc(1).energy(i)
               c2(i) =   control_toadc(1).nphe_mip(i)
     +              / control_toadc(1).dep_mip(i)
               c3(i) = c1(i) / c2(i)
               control_toadc(1).c1(i) = c1(i)
               control_toadc(1).c2(i) = c2(i)
               control_toadc(1).c3(i) = c3(i)         
               write(6,3) ems_control(1).sampfrac(i)
     +              , ems_control(1).adcbits(i)
     +              , control_toadc(1).energy(i)
     +              , control_toadc(1).nphe_mip(i)
     +              , control_toadc(1).dep_mip(i)
     +              , c1(i), c2(i), c3(i)
            endif
         enddo
      endif

      adc_emc1_h.nok = 0
      adc_emc2_h.nok = 0
      o1  = 0  ! Number of entry to ADC array
      o2  = 0  ! Number of entry to second ADC array

      do ih=1,hits_emc_h.nok 

         det=hits_emc(ih).det
         if(det.lt.1 .or. det.gt.MAXDET) then
            write(*,*)'***dep_e_toadc ERROR: hits_emc.det is out of range'
     +           ,ih,det
            return
         endif

         mode=control_toadc(1).mode(det)
         if(mode .eq. 0) then ! No ADC
            adc  = -1
            
         elseif(mode .eq. 1) then ! DE => ADC
            radc = hits_emc(ih).energy * control_toadc(1).c1(det)
            adc  = int (radc+0.5) ! Nearest positive integer number  
            
         elseif(mode .eq. 2) then ! DE => PHE => ADC
            amu  = hits_emc(ih).energy * control_toadc(1).c2(det)
            if(amu.gt.0.0) then
               call poissn(amu, nphe, ierr) !   See mathlib
               radc = float(nphe) * control_toadc(1).c3(det)
               adc     = int (radc+0.5) ! Nearest positive integer number  
            else
               adc  = 0
            endif               ! if(amu.gt.0.0)
            
         else
            write(6,*)
     +        '***dep_e_toadc ERROR:Bad value control_toadc(1).mode='
     +        ,mode
            return
         endif                  ! mode .eq. 0,1,2
         
         adc = min(ems_control(1).adcbits(det),adc)
             
         if(control_toadc(1).zero_sup(det).eq.1
     +        .and. adc.le.0) then 
            goto 100
         endif
         
         if(det.eq.BEMC .or. det.eq.BSMDE .or.
     +      det.eq.EEMC .or. det.eq.ESMDE) then
            o1 = o1 + 1
            adc_emc1(o1).det     = det       ! EMC
            adc_emc1(o1).module  = hits_emc(ih).module
            adc_emc1(o1).eta     = hits_emc(ih).eta   
            adc_emc1(o1).sub     = hits_emc(ih).sub 
            adc_emc1(o1).adc     = adc
            if(adc.eq.-1)then
               adc_emc1(o1).energy  = hits_emc(ih).energy
            else
               adc_emc1(o1).energy  = 0.0
            endif
         else
            o2 = o2 + 1
            adc_emc2(o2).det     = det        ! EMC
            adc_emc2(o2).module  = hits_emc(ih).module
            adc_emc2(o2).eta     = hits_emc(ih).eta   
            adc_emc2(o2).sub     = hits_emc(ih).sub 
            adc_emc2(o2).adc     = adc
            if(adc.eq.-1)then
               adc_emc2(o2).energy  = hits_emc(ih).energy
            else
               adc_emc2(o2).energy  = 0.0
            endif
         endif

 100     continue
      enddo 

      if(o1 .gt. adc_emc1_h.maxlen) then
         write(6,4) adc_emc1_h.maxlen , o1
         return
      endif
      if(o2 .gt. adc_emc2_h.maxlen) then
         write(6,4) adc_emc2_h.maxlen , o2
         return
      endif

      adc_emc1_h.nok=o1
      adc_emc2_h.nok=o2
      dep_e_toadc = STAFCV_OK
      return

 1    format('dep_e_toadc(Ver. 1.20) date => 4-now-1998')
 2    format('dep_e_toadc: detector# = ',i2,' mode =',i2)
 3    format(
     + ' Sample Fraction coefficient                  ', f10.4/
     + ' Barrel Emc Tower ADC Bits                    ', i10/
     + ' Max energy in EMC towers                    ', f10.2/
     + ' Mean Value of Number of PHE for MIP in EMC  ', f10.2/
     + ' Mean Value of DE for MIP in EMC             ', f10.6/
     + ' Coeff. transition from Deposit Energy to ADC ', f10.4/
     + ' Coeff. transition from Deposit Energy to PhE ', f10.4/
     + ' Coeff. transition from PhE            to ADC ', f10.4/)
 4    format('***dep_e_toadc ERROR: Not enough memory in adc_emc1:'
     + ,i5,' available, ',i6,' needed') 
 5    format('***dep_e_toadc ERROR: Not enough memory in adc_emc2:'
     + ,i5,' available, ',i6,' needed') 
      end
