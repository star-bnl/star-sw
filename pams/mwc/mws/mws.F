CC:>--------------------------------------------------------------------
CC: FILE:       mws.F.template
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION MWS(
     1              mhit_h,              mhit ,
     3              geom_h,              geom ,
     4               cal_h,               cal ,
     5             mwpar_h,             mwpar ,
     6            mevent_h,            mevent ,
     7            sector_h,            sector ,
     8               raw_h,               raw ) 
      IMPLICIT NONE
#include "mws.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    MWS
CC: DESCRIPTION: Simulation package for MWC where both hit counting
CC:              and charge integration can be performed.             
CC:        Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:             from mws.idl.
CC:             Please edit comments and code.
CC: AUTHOR:     jpw - James Whitfield whitfield@cmchem.chem.cmu.edu
CC: ARGUMENTS:
CC:          IN:
CC:       INOUT:
CC:             mhit     - geant hit table for mwc
CC:             geom     - geometry definition table for MWC
CC:              cal     - calibarion table
CC:            mwpar     - input parameters for simulation
CC:           mevent     - event table for simulation
CC:           sector     - sector results
CC:              raw     - adc output output table for MWC sim.
CC:         OUT:
CC:>------------------------------------------------------------------
*--
      INTEGER    MWG, Igeo, MWS_DERES
*--
*--    Initialize geometry if it is not already done
*--
       if ( Geom(1).init .eq. 0 ) then
          Igeo = MWG ( Geom_h,    Geom
     +,                     Cal_h,    Cal )
       endif
*--
*--    Simulate the detector response
*--
       mws = MWS_DERES ( Mhit_h,   Mhit
     +,                  Geom_h,   Geom
     +,                  Cal_h,    Cal
     +,                  Mwpar_h,  Mwpar
     +,                  Mevent_h, Mevent
     +,                  Sector_h, Sector
     +,                  Raw_h,    Raw   )
*--
*--     That's it
*--
       end 
****************************************************************************
*     Routine to simulate the MWC response
****************************************************************************
      INTEGER FUNCTION MWS_DERES ( Mhit_h,   Mhit
     +,                            Geom_h,   Geom
     +,                            Cal_h,    Cal
     +,                            Mwpar_h,  Mwpar
     +,                            Mevent_h, Mevent
     +,                            Sector_h, Sector  
     +,                            Raw_h,    Raw    )
      IMPLICIT NONE
C-----------------------------------------------------------------------
#include "mws.inc"
C---------------------------------------------------------------------
*--
      INTEGER Mwc_raw_len
      PARAMETER (Mwc_raw_len=8000)
      INTEGER Ihit, Isec, Id, ieta        
      INTEGER Ir, Iphi, Index, Num_hits
      INTEGER Nsignal, Nhit(Mwc_raw_len)
      INTEGER num_wires_sect, num_wires_adc, num_adcs_out
      INTEGER icell, cell_num, cell_hit(7680)
      INTEGER mwc_adc_hit(384), overload_slat
      INTEGER hit_index, tot_cell_hit(384), ee_correct
      INTEGER isect, iosect, i, j, wire_hit(7680), ipie
      REAL    Tolerance, Pi, mwc_adc_de(384)
      REAL    Rhit, Phihit, Phi_step
      REAL    Scale, RG32, Dummy, Ped
      REAL    Total_de(Mwc_raw_len), gain
      REAL    outsect_ir, insect_ir, wire_step
      REAL    phicent, phicos, sector_ir
      REAL    local_hit, local_step, cell_de(7680)
      LOGICAL First, wires_set
      SAVE    Tolerance
      SAVE    First      
      SAVE    Pi
*--
      DATA    Tolerance / -0.1 /
      DATA    First     / .true. /
      DATA    Pi        / 3.141592654 /
*--
      if ( First ) then
         First   = .false.
*--    Default this works
*--
      MWS_DERES = STAFCV_OK

*--
*--     Check whether doing it counting or de sums
*--     IF Mwpar.wires = 1 then do wire hit counting
*--
         if (Mwpar(1).wires .eq. 1) then
            wires_set = .true.
         else
            wires_set = .false.
         endif
*-- 
*--    Set the gain for ADC output from MWC
*--

         if (Mwpar(1).gain .eq. 0.0)  then
          write (6,*) 'WARNING -- Gains not set'
          write (6,*) 'Setting gain to 1.0!'
          Mwpar(1).gain = 1.0
         endif
         gain = Mwpar(1).gain
         Scale = 2.5e5*gain
*--
*--      Let's set some constants !!!
*--      outsect_ir   :  outer sector radius
*--      insect_ir    :  inner sector radius
*--      wire_step    : distance between wires in cm
*--      num_wires_sect : Number of wires per sector (inner/outer)
*--      num_adcs_out : Number of adc values for MWC
*--      num_wires_adc: Number of wires used per adc
*--     
         outsect_ir     = Geom(1).r2min
         insect_ir       = Geom(1).r1min
         wire_step      = 0.4
         num_wires_sect = 160
         num_adcs_out   = Mwpar(1).num_adcs_out
         num_wires_adc  = Mwpar(1).num_wires_adc
*--
*--    Check that the endcap wire segments have been set correctly
*--
        ee_correct = abs(7680 - num_adcs_out * num_wires_adc)
        if (ee_correct .gt. 0) then
           write (6,*) 'Due to GSTAR geometry there must be -- 7680'
           write (6,*) 'total wires. Correct the error and restart!'
           MWS_DERES = STAFCV_BAD
           return
        endif
*--
*--    Check that geometry set correctly
*--
         if ( Geom(1).neta .eq. 0 ) then
            write (6,*) ' Geometry NOT DEFINED '
            MWS_DERES = STAFCV_BAD
            return
         endif
*--
*--   Get sector length in phi
*--
         if ( Geom(1).nphi .gt. 0 ) then
            Phi_step  = 2 * Pi / Geom(1).nphi
         else
            write (6,*) 'MWS: Geometry not initialized '
            MWS_DERES = STAFCV_BAD
            return
         endif
      endif 
*--
      call VZERO ( Total_de, Mwc_raw_len )
      call VZERO ( Nhit    , Mwc_raw_len )
*--
*--    Zero event hit counter
*--
      Mevent(1).no_hits = 0
      overload_slat = 0

      do j = 1,num_adcs_out*num_wires_adc
         cell_hit(j) = 0
         cell_de(j)  = 0
         wire_hit(j) = 0
      enddo
       
*--
*--    Check if any data
*--
      if ( mhit_h.nok .gt. 0 ) then
*--
*--    Loop over hits
*--
*--
         do Ihit = 1, mhit_h.nok
*--
            Rhit   = sqrt(Mhit(Ihit).x(1)**2+Mhit(Ihit).x(2)**2)
            if (   (Rhit .gt. Geom(1).r1min) 
     +.and.        (Rhit .lt. Geom(1).r2max+10.0)  ) then
*--
               Mevent(1).no_hits = Mevent(1).no_hits + 1
               if (Mhit(Ihit).track_p .ne. Mhit(Ihit-1).track_p) then
                  Mevent(1).no_tracks = Mevent(1).no_tracks + 1
               endif 
*--
*--    Get Phi index now
*--
               Phihit = ATAN2(Mhit(Ihit).x(2),Mhit(Ihit).x(1))
               if ( Phihit .lt. 0 ) Phihit = Phihit + 2*Pi
*--
*--     Get necessary geometry information
*--
*--            Get phi sector number
*--
               ipie = int(phihit/(phi_step/2.))+1
               isect = nint(((float(ipie)-30.0)/-2.0)+0.2)
               if (isect .gt. 12) isect = isect - 12
*--
*--            Get the hit position and angle with respect to 
*--            center of sector
*--
               iphi = isect
               phicent = -phi_step*(iphi)+ 2.5 * pi
               if (phicent .ge. 2*pi) phicent = phicent - (2*pi)
               phicos = cos(abs(phihit-phicent)) 
*--
*--            Determine whether hit was in inner or outer sector
*--
               if (rhit .lt. 120.0) then
                  iosect = 1
               else
                  iosect = 2
               endif

               if (iosect.eq.1) then
                  sector_ir = insect_ir/phicos
               else
                  sector_ir = outsect_ir/phicos
               endif
*--
*--     Determine hit information locally for each cell
*--

               local_hit  = rhit - sector_ir
               local_step = wire_step/phicos
               icell      = int(local_hit/local_step) + 1
               cell_num = (((isect-1)*(2*num_wires_sect))
     1                    +((iosect-1)*num_wires_sect)+icell)
          if (mhit(ihit).x(3) .gt. 0.0) then 
            cell_num = cell_num +(num_wires_sect*24)
          endif
*--            
*--     Get the number of hits in a cell and the total energy
*--     deposition in the cell
*--
               cell_hit(cell_num) = cell_hit(cell_num) + 1
               cell_de(cell_num)  = cell_de(cell_num) + mhit(ihit).de
            else
               write (6,*) 'MWS : Wrong geometry '
               write (6,*) mhit(ihit).x(1), mhit(ihit).x(2), mhit(ihit).x(3)
            endif
*--    end loop over hits
         enddo
*--
*--
*--    Get wire hit information
*--
       do i = 1, (num_wires_sect * 24 * 2)
        if (cell_de(i) .ge. mwpar(1).de_thresh) then
         if (cell_hit(i) .ge. 1) then
            wire_hit(i) = 1
         endif
        endif
       enddo 
        
*--
*--
*--
       Mevent_h.nok = 1
       do i = 1,num_adcs_out
          mwc_adc_hit(i) = 0
          mwc_adc_de(i)  = 0
          tot_cell_hit(i) = 0
       enddo
*--
*--    Loop over sectors now
*--
         Nsignal = 0
         do Isec = 1, num_adcs_out
           do j = 1, num_wires_adc
             hit_index = (isec-1)*num_wires_adc + j
             
             tot_cell_hit(isec) = tot_cell_hit(isec) + cell_hit(hit_index)
             mwc_adc_hit(isec)  = mwc_adc_hit(isec)  + wire_hit(hit_index)
             mwc_adc_de(isec)   = mwc_adc_de(isec) + cell_de(hit_index)
           enddo
*--
             iphi = int((float(isec)/16.0)-0.01) +  1
             ieta = isec - ((iphi-1) * 16)
             if (iphi .gt. 12) iphi = iphi - 12
*--
             if (mwc_adc_hit(Isec) .gt. 0.) then
               nsignal = nsignal + 1
*--
               sector(nsignal).iphi = iphi
               sector(nsignal).ieta = ieta
               sector(nsignal).sector = isec
               sector(nsignal).nhit   = mwc_adc_hit(isec)
               sector(nsignal).de     = mwc_adc_de(isec)
               sector(nsignal).tot_hit = tot_cell_hit(isec)
*--
               raw(nsignal).sector = isec
               if (wires_set .eq. .true.) then
                 raw(nsignal).adc = mwc_adc_hit(isec)
               else
                 raw(nsignal).adc = mwc_adc_de(isec)*Scale
                  if (raw(nsignal).adc .gt. 32) then
                     raw(nsignal).adc = 32
                     overload_slat = overload_slat + 1
                     write (6,*) 'Slat Number ', nsignal,' ADC Value Overflow'
                     write (6,*) 'Total Overflow Slats = ', overload_slat
                  endif
               endif
           endif
         enddo
*--
*--    Define the number of slats with raw data
*--
         Raw_h.nok    = Nsignal
         Sector_h.nok = Nsignal
*--
*--    end if any data
*--
      endif 
*--
*--    That's It
*--
  990 continue
      end
