C=======================================================================
      INTEGER FUNCTION RL0      ( ctb_geo_h,    ctb_geo
     +,                           mwc_geo_h,    mwc_geo
     +,                           ctb_raw_h,    ctb_raw
     +,                           mwc_sector_h, mwc_sector
     +,                           mwc_raw_h,    mwc_raw
     +,                           ctr_h,        ctr
     +,                           L0_h,         L0
     +,                           ctbcal_h,     ctbcal
     +,                           mwccal_h,     mwccal )
      IMPLICIT NONE

#include "rl0.inc"
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
*--
*--      CTB 
*--
      INTEGER    Max_ctb_phi, Max_ctb_eta, max_div_ctb, ifit
      INTEGER    Raw_ctb(420,18), iloop
      REAL       ctb_mult, sum_ctb 
      LOGICAL    First
      SAVE       First
      DATA       First    / .true. /
*--
*--     MWC
*--
      INTEGER    Max_mwc_phi, Max_mwc_eta   
      INTEGER    Raw_mwc(2,12,320) 
      INTEGER    iall, iset, isum, id
      INTEGER    i, iphi, ieta, idiv, irun, ineta
      INTEGER    isector, iz, index, ngran, eta
      REAL       forward, backward
      REAL       mwc_mult,gain, sum, sum_mwc
      REAL       max_div_mwc, fwh, fact
*--
*--   SET VARIOUS CONSTANTS FOR THE RUNS
*--
      write (6,*) 'Im In!'
       if ( First ) then
          First = .false.
*--
         if   (Mwccal(1).fit(1) .eq. 0 
     + .or.    Mwccal(1).fit(2) .eq. 0) then
          write (6,*) 'MWC:No Calibration File'
          RL0 = STAFCV_BAD
          return
         endif
         if (Ctbcal(1).fit(1) .eq. 0 
     + .or.  Ctbcal(1).fit(2) .eq. 0) then
          write (6,*) 'CTB:No Calibration File'
          RL0 = STAFCV_BAD
          return
         endif
*--
*--      ctr.det_set sets detector to CTB(1) or TOF(2)
*--      ctr.segment sets method of summing 
*--      ctr.ctb(mwc)_pixel_out sets number of output pixels
*--      for the CTB (MWC)
*-- 
         if (ctr(1).det_set .eq. 0) ctr(1).det_set = 1
         if (ctr(1).ctb_pixel_out .eq. 0) ctr(1).ctb_pixel_out = 16
         if (ctr(1).mwc_pixel_out .eq. 0) ctr(1).mwc_pixel_out = 16
*--
         Max_ctb_phi = ctb_geo(1).n_tray_phi * ctb_geo(1).n_counter_phi
         Max_ctb_eta = ctb_geo(1).n_tray_eta * ctb_geo(1).n_counter_eta
         Max_mwc_eta = mwc_geo(1).neta
         Max_mwc_phi = mwc_geo(1).nphi

         write (9,*) max_ctb_phi, max_ctb_eta, max_mwc_phi, max_mwc_eta

*--
*--
       endif
*--
*--     Set arrays to zero
*--
      do ieta = 1, Max_ctb_eta
       do iphi = 1, Max_ctb_phi
        Raw_ctb(iphi,ieta) = 0
       enddo
      enddo
*--
      do iz = 1, 2
       do ieta = 1, Max_mwc_eta
        do iphi = 1, Max_mwc_phi
         Raw_mwc ( iz, iphi, ieta ) = 0
        enddo
       enddo
      enddo  
*--
*--
*--     Fill CTB array
*--
      if (Ctb_raw_h.nok .gt. 0) then
        do i = 1, Ctb_raw_h.nok
           iphi = Ctb_raw(i).i_phi
           ieta = Ctb_raw(i).i_eta
            if (iphi .ge. 1 .and. iphi .le. Max_ctb_phi
     + .and.     ieta .ge. 1 .and. ieta .le. Max_ctb_eta) then
                 
             Raw_ctb (iphi,ieta) = Ctb_raw(i).adc
             write (9,*) 'CTB ', iphi, ieta, Raw_ctb(iphi,ieta)
            else
             write ( 6, * ) 
     +       iphi, ' or ',ieta, ' are too high an index '
            endif
         enddo
      endif
*--
*--
*--     Fill MWC array
*--
      if (mwc_raw_h.nok .gt. 0) then
       do i = 1, mwc_raw_h.nok
        if (mwc_raw(i).sector .le. 192) then
          iz = 1
        else
          iz = 2
        endif
       ieta = mwc_sector(i).ieta
*--
*--    This puts the positive eta MWC index in proper eta-phi 
*--    space for summing
*--
*--        if (iz .eq. 2) ieta = (mwc_geo(1).neta) + ieta
*--
*--
*--
        iphi = mwc_sector(i).iphi 
*--
         if (iphi .ge. 1 .and. iphi .le. Max_mwc_phi  
     + .and.     ieta .ge. 1 .and. ieta .le. 2*Max_mwc_eta 
     + .and.     iz   .ge. 1 .and. iz   .le. 2           ) then

************************************************************
**** There is a SERIOUS problem here. mwc_raw(i).adc DOES  *
**** NOT EXIST, as far as I can tell. I commented the line *
**** out and added mwc_raw(i).count just to get rl0 to     *
**** compile. This will need to be checked thoroughly to   *
**** make sure code is behaving correctly. -ZM 2-16-98     *
************************************************************
*          Raw_mwc (iz,iphi,ieta) = mwc_raw(i).adc
          Raw_mwc (iz,iphi,ieta) = mwc_raw(i).count
          write (9,*) 'MWC ',iz, iphi, ieta, raw_mwc(iz,iphi,ieta)
           else
          write ( 6, * ) isector,index,
     +    iz,' or ',iphi, ' or ',ieta, ' are too high an index '
         endif
        enddo
      endif
*--
*--
*--  PUT IN MULTIPLICITY DATA WORDS FOR ETA-PHI PIXEL SPACE
*--  FOR THE CTB (LUT)
*--
      iset = 0
      max_div_ctb = 4
      do ieta = 1, Max_ctb_eta
      do idiv = 1, max_div_ctb
       sum = 0
        do iphi = 1+(idiv-1)*(max_ctb_phi/max_div_ctb), 
     +            idiv * (Max_ctb_phi/max_div_ctb)
            ctb_mult = 0
            ctb_mult = (Raw_ctb(iphi,ieta)-ctbcal(1).fit(1))/
     +                  ctbcal(1).fit(2)
            if (ctb_mult .le. 0) ctb_mult = 0
            sum = sum + ctb_mult
         enddo
        iset = iset + 1
        L0(1).fpga1_ctb(iset) = sum
        write (19,*) iset, 'CTB ', L0(1).fpga1_ctb(iset)
      enddo
      enddo 
*--
*--
*--
*--    VARIABLE PIXEL SPACE FOR MWC SUMMING IS PERFORMED OVER ETA REGIONS
*--
        iset = 0
        do iz = 1,2
        do ieta = 0, max_mwc_eta-2, 2
         sum =0
          do idiv = 1,2
          do iphi = 1,max_mwc_phi   
           mwc_mult=0
           eta = ieta + idiv
           fwh = raw_mwc(iz,iphi,eta)/20.0
           if (raw_mwc(iz,iphi,eta) .ge. 16) fwh = 0.80
           write (20,*) fwh, raw_mwc(iz,iphi,eta)
           mwc_mult = 20*(-(alog(1-fwh)))
           write (20,*) mwc_mult
             if (eta .le. 8) then
              fact = 1.231 + 0.036*eta
             else
              fact = 1.55 + 0.068*eta
             endif
           mwc_mult = mwc_mult/fact
           if (mwc_mult .lt. 0) mwc_mult = 0
           write (18,*) iz, iphi, eta, mwc_mult, sum
           sum = sum + mwc_mult
          enddo
          enddo
          iset = iset + 1
          L0(1).fpga1_mwc(iset) = sum
          write (18,*) iset, 'MWC ',L0(1).fpga1_mwc(iset)
          write (19,*) iset, 'MWC ', L0(1).fpga1_mwc(iset)
         enddo
        enddo
*--
*--
*--   DSM SUMMING FOR MWC (SUM OVER PHI)
*--
       do iloop = 1,2
        sum = 0
        do irun = 1,ctr(1).mwc_pixel_out/2
            iset=((iloop-1)*ctr(1).mwc_pixel_out/2) + irun
         sum = sum + L0(1).fpga1_mwc(iset)
        write (22,*) 'MWC',iset,l0(1).fpga1_mwc(iset),sum
        enddo
        gain = ctr(1).fpga_gain(1)
        L0(1).mwc_tot(iloop) = sum * gain
       enddo
         write (22,*) 'mwc_tot ', l0(1).mwc_tot(1), l0(1).mwc_tot(2) 
*--
*--   DSM SUMMING FOR CTB (SUM OVER PHI)
*--
       do iloop = 1, 2
        sum = 0
         do iall = 1, ctr(1).ctb_pixel_out/2
          iset = ((iloop-1)*ctr(1).ctb_pixel_out/2) + iall
          sum = sum + L0(1).fpga1_ctb(iset)
          write (22,*) 'CTB',iset,l0(1).fpga1_ctb(iset),sum
          enddo 
         gain = ctr(1).fpga_gain(1)
        L0(1).ctb_tot(iloop) = sum * gain 
       enddo
          write (22,*) 'ctb_tot ',l0(1).ctb_tot(1), l0(1).ctb_tot(2)
*--
*--    MWC/CTB ratio and Total Multiplicity
*--
       do iloop = 1,2
        sum_ctb = sum_ctb +  L0(1).ctb_tot(iloop)
        sum_mwc = sum_mwc +  L0(1).mwc_tot(iloop)
       enddo
       L0(1).mwc_ctb = sum_mwc/sum_ctb
       L0(1).tot_mul   = sum_mwc + sum_ctb
*--
*--    F/B ratio
*--
        forward  = L0(1).ctb_tot(1) + L0(1).mwc_tot(1)
        backward = L0(1).ctb_tot(2) + L0(1).mwc_tot(2) 
        L0(1).fob = forward/backward            
          write (22,*) l0(1).mwc_ctb, l0(1).tot_mul, l0(1).fob
*--
*--
      L0_h.nok = 1
      rl0 =  STAFCV_OK

      write (6,*) 'Im out'
*--
*--    That's it
*--
      end




