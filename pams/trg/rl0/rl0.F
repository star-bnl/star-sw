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
      INTEGER    Max_ctb_phi, Max_ctb_eta 
      INTEGER    ctb_div_phi, ctb_div_eta
      INTEGER    Raw_ctb(420,18), iloop
      REAL       ctb_mult, sum_ctb 
      LOGICAL    First
      SAVE       First
      DATA       First    / .true. /
*--
*--     MWC
*--
      INTEGER    Max_mwc_phi, Max_mwc_eta   
      INTEGER    mwc_div_phi, mwc_div_eta
      INTEGER    Raw_mwc(2,12,320) 
      INTEGER    iall, iset, isum, id
      INTEGER    i, iphi, ieta, idiv, irun, ineta
      INTEGER    isector, iz, index, ngran, eta
      INTEGER    set_phi,set_eta
      REAL       forward, backward
      REAL       mwc_mult,gain, sum, sum_mwc
      REAL       fwh, fact
*--
*--   SET VARIOUS CONSTANTS FOR THE RUNS
*--


      if ( First ) then
         First = .false.
*--
         if   (Mwccal(1).fit(1) .eq. 0 
     +         .or.    Mwccal(1).fit(2) .eq. 0) then
            write (6,*) 'MWC:No Calibration File',Mwccal(1).fit(1), Mwccal(1).fit(2)
            RL0 = STAFCV_BAD
            return
         endif
         if (Ctbcal(1).fit(1) .eq. 0 
     +       .or.  Ctbcal(1).fit(2) .eq. 0) then
            write (6,*) 'CTB:No Calibration File'
            RL0 = STAFCV_BAD
            return
         endif
*--
*--
         Max_ctb_phi = ctb_geo(1).n_tray_phi * ctb_geo(1).n_counter_phi
         Max_ctb_eta = ctb_geo(1).n_tray_eta * ctb_geo(1).n_counter_eta
         Max_mwc_eta = mwc_geo(1).neta
         Max_mwc_phi = mwc_geo(1).nphi

*--         write (9,*) Max_ctb_phi, Max_ctb_eta, Max_mwc_phi, Max_mwc_eta

*--
*--      set default value for the control table
*--      for mwc 4X4 (4pixels in phi and 4 in eta)
*--      for ctb 4X4 
*-- 
         mwc_div_phi = ctr(1).mwc_eta_pixels
         mwc_div_eta = ctr(1).mwc_phi_pixels
         ctb_div_phi = ctr(1).ctb_eta_pixels
         ctb_div_eta = ctr(1).ctb_phi_pixels

         if (mwc_div_phi.eq.0 .or. (mwc_div_eta.eq.0)) then
            mwc_div_phi = 4
            mwc_div_eta = 4
         endif
         if (ctb_div_phi.eq.0 .or. (ctb_div_eta.eq.0)) then
            ctb_div_phi = 4
            ctb_div_eta = 4
         endif
*--
*--      Both of the endcaps are divided in eta regions so
*--      the if there are 4 total eta division 2 for front+ 2 for back         
*--
         mwc_div_eta = mwc_div_eta/2
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
         do ieta = 1, 320
            do iphi = 1, 12
               Raw_mwc ( iz, iphi, ieta ) = 0
            enddo
         enddo
      enddo  

      do iset = 1, 64
           L0(1).fpga1_ctb(iset) = 0 
           L0(1).fpga1_mwc(iset) = 0 
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
     +          .and. ieta .ge. 1 .and. ieta .le. Max_ctb_eta) then
                 
               Raw_ctb (iphi,ieta) = Ctb_raw(i).adc
*--               write (9,*) 'CTB ', iphi, ieta, Raw_ctb(iphi,ieta)
            else
*--               write ( 6, * ) iphi, ' or ',ieta, ' are too high an index '
            endif
         enddo
      endif
*--
*--
*--     Fill MWC array
*--
*-- 2/8/99 d.e. russ changed (sector .le. 192) to (sector .le. 48)
*--        (no longer 16 sets of 20 wires but 4 sets of 80 wires/sector)
*--
      if (mwc_raw_h.nok .gt. 0) then
*--         write (9,*) 'MWC:... ', mwc_raw_h.nok
         do i = 1, mwc_raw_h.nok
            if (mwc_raw(i).sector .le. 48) then
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
     +      .and. ieta .ge. 1 .and. ieta .le. 2*Max_mwc_eta 
     +      .and. iz   .ge. 1 .and. iz   .le. 2 ) then
               Raw_mwc (iz,iphi,ieta) = mwc_raw(i).count
*               write (9,*) 'MWC ',iz, iphi, ieta, raw_mwc(iz,iphi,ieta)
            else
*               write ( 6, * ) 'MWC:',isector,index,
*     +         iz,' or ',iphi, ' or ',ieta, ' are too high an index '
            endif
         enddo
      endif
*--
*--
*--  PUT IN MULTIPLICITY DATA WORDS FOR ETA-PHI PIXEL SPACE
*--  FOR THE CTB (LUT)
*--
      iset = 0
      do set_phi = 1, ctb_div_phi
        do set_eta = 1, ctb_div_eta
           sum = 0
           do iphi = 1+(set_phi-1)*(Max_ctb_phi/ctb_div_phi), 
     +        set_phi * (Max_ctb_phi/ctb_div_phi)
              do ieta = 1+(set_eta-1)*(Max_ctb_eta/ctb_div_eta),
     +           set_eta * (Max_ctb_eta/ctb_div_eta)     
                 ctb_mult = 0
                 ctb_mult = (Raw_ctb(iphi,ieta)-ctbcal(1).fit(1))/
     +                      ctbcal(1).fit(2)
                 if (ctb_mult .le. 0) ctb_mult = 0
                 sum = sum + ctb_mult
*                 write(17,*) 'CTB: eta',ieta,'   phi',iphi,
*     +               '  set eta',set_eta,' set phi',set_phi,
*     +               '  net set',iset+1,'  sum:',sum
              enddo
           enddo
           iset = iset + 1
           L0(1).fpga1_ctb(iset) = sum
*           write (19,*) iset, 'CTB ', L0(1).fpga1_ctb(iset)
        enddo
      enddo 
*--
*--
*--
*--    VARIABLE PIXEL SPACE FOR MWC SUMMING IS PERFORMED OVER ETA REGIONS
*--
      iset = 0
      do iz = 1,2
         do set_phi = 1, mwc_div_phi
            do set_eta = 1, mwc_div_eta
               sum = 0
               do iphi = 1+(set_phi-1)*(Max_mwc_phi/mwc_div_phi),
     +            set_phi * (Max_mwc_phi/mwc_div_phi)
                  do ieta = 1+(set_eta-1)*(Max_mwc_eta/mwc_div_eta),
     +               set_eta * (Max_mwc_eta/mwc_div_eta)
                     fwh = raw_mwc(iz,iphi,ieta)/80.0
                     if (raw_mwc(iz,iphi,ieta) .ge. 64) fwh = 0.80
                     mwc_mult = 80*(-(alog(1-fwh))) 
                     if (ieta .le. 2) then
                        fact = 1.231 + 0.144*ieta
                     else
                        fact = 1.55 + 0.272*ieta
                     endif
                     mwc_mult = mwc_mult/fact
                     if (mwc_mult .lt. 0) mwc_mult = 0
                     sum = sum + mwc_mult
*                     write(17,*) 'MWC: eta',ieta,'   phi',iphi,
*     +                  '  set eta',set_eta,' set phi',set_phi,
*     +                  '  net set',iset+1,'  sum:',sum
                  enddo
               enddo
               iset = iset + 1
               L0(1).fpga1_mwc(iset) = sum
*               write (19,*) iset, 'MWC ', L0(1).fpga1_mwc(iset)
            enddo
         enddo
      enddo
*--
*--
*--   DSM SUMMING FOR MWC (SUM OVER PHI)
*--
      do iz = 1, 2
         sum=0
         do iset = (iz-1)*mwc_div_phi*mwc_div_eta+1,
     +      iz*mwc_div_phi*mwc_div_eta
            sum = sum + L0(1).fpga1_mwc(iset)
*            write (22,*) 'MWC',iset,l0(1).fpga1_mwc(iset),sum
         enddo
         gain = ctr(1).fpga_gain(1)
         L0(1).mwc_tot(iz) = sum * gain
      enddo
*      write (22,*) 'mwc_tot ', l0(1).mwc_tot(1), l0(1).mwc_tot(2) 
*--
*--   DSM SUMMING FOR CTB (SUM OVER PHI)
*--
      do iloop = 1, 2
         sum = 0
         do iset = (iloop-1)*ctb_div_phi*(ctb_div_eta/2)+1,
     +      iloop*ctb_div_phi*(ctb_div_eta/2)
            sum = sum + L0(1).fpga1_ctb(iset)
*            write (22,*) 'CTB',iset,l0(1).fpga1_ctb(iset),sum
         enddo 
         gain = ctr(1).fpga_gain(1)
         L0(1).ctb_tot(iloop) = sum * gain 
      enddo
*      write (22,*) 'ctb_tot ',l0(1).ctb_tot(1), l0(1).ctb_tot(2)
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
*      write (22,*) l0(1).mwc_ctb, l0(1).tot_mul, l0(1).fob
*--
*--
      L0_h.nok = 1
      rl0 =  STAFCV_OK


*--
*--    That's it
*--
      end




