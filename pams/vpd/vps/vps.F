CC:>--------------------------------------------------------------------
CC: FILE:       vps.F.template
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION VPS(
     1            gvhit_h,               gvhit ,
     2            vpar_h,                vpar ,
     3            vsim_h,                vsim ,
     4            vraw_h,                vraw ) 
      IMPLICIT NONE
#include "vps.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    VPS
CC: DESCRIPTION: Simulates VPD detector response.
CC:        
CC:        
CC:        
CC:           
CC:            
CC: AUTHOR:     zm  - Z. Milosevich, milosevich@cmchem.chem.cmu.edu
CC: ARGUMENTS:
CC:          IN:
CC:       INOUT:
CC:           gvhit       - g2t_vpd_hit table from g2t
CC:         gvhit_h       - Header Structure for g2t_vpd_hit table
CC:           vpar        - parameter settings vps_par table
CC:         vpar_h        - Header Structure for vps_par
CC:           vsim        - simulated physics quantities
CC:         vsim_h        - Header Structure for vps_sim
CC:           vraw        - simulated adc and tdc values
CC:         vraw_h        - Header Structure for vps_raw
CC:         OUT:
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------
      INTEGER ihit, test, nok
      INTEGER ierr

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     =====================
      INTEGER I,J,K
*
*
*     variables associated with time response
*
*     time       time between vertex and first photons at pmt
*     vpd_tim    time response for each photo-tube
*
      REAL random1,random2
      REAL tim(48)
      REAL vpd_tim(48)
*
*     kinematic variables for calculating adc response
*
*     beta       particle velocity
*     c_theta    cerenkov angle
*     p          total momentum
*
      REAL  beta, c_theta, p
*
*     variables for calculating ADC response
*
*     n_photons      number of photons produced in hit
*     tot_photons     total # photons in pmt/event
*
      REAL n_photons
      REAL tot_photons (48) 
*
*
*     Hit parameters
*
*     vpd_x          x position 
*     vpd_y          y position
*     vpd_z          z position
*     vpd_Px         x momentum
*     vpd_Py         y momentum
*     vpd_Pz         z momentum
*     vpd_e_tot      total energy of particle
*     vpd_T_length   track length inside detector volume
*     vpd_Tof        time of flight up to entrance of detector
*     vpd_dE         energy loss in detector
*     vpd_numbv      division number for VPD
*     vpd_iudet      inner or outer ring of VPD
*
      REAL vpd_x, vpd_y, vpd_z, vpd_Px, vpd_Py, vpd_Pz, 
     +     vpd_e, vpd_T_length, vpd_Tof, vpd_dE, 
     +     vpd_numbv, vpd_iudet        
*
      REAL clt, ref_in, t_rad, t_res, kn, arang, trang, pthresh
      REAL mphot, mtime
      INTEGER npmt, ipmt, div_number
C
C     Data Statements:
C     ================
C
C     Executable Statements:
C=======================================================================
C
      if ( vpar(1).n_pmt .eq. 0 ) vpar(1).n_pmt = 48
      if ( vpar(1).c_light .eq. 0 ) vpar(1).c_light = 2.99792458e10
      if ( vpar(1).refractive_index .eq. 0 ) 
     &     vpar(1).refractive_index = 1.46
      if ( vpar(1).thick_rad .eq. 0 ) vpar(1).thick_rad = 0.635
      if ( vpar(1).time_res .eq. 0 ) vpar(1).time_res = 75.0e-12
      if ( vpar(1).adc_range .eq. 0 ) vpar(1).adc_range = 1024
      if ( vpar(1).tdc_range .eq. 0 ) vpar(1).tdc_range = 1024
      if ( vpar(1).konst .eq. 0 ) vpar(1).konst = 1300
      if ( vpar(1).phot_thresh .eq. 0 ) vpar(1).phot_thresh = 5
      if ( vpar(1).max_photons .eq. 0 ) vpar(1).max_photons = 10e8
      if ( vpar(1).max_time .eq. 0 ) vpar(1).max_time = 100e-9

      npmt    = vpar(1).n_pmt
      clt     = vpar(1).c_light
      ref_in  = vpar(1).refractive_index
      t_rad   = vpar(1).thick_rad
      t_res   = vpar(1).time_res
      arang   = vpar(1).adc_range
      trang   = vpar(1).tdc_range
      kn      = vpar(1).konst
      pthresh = vpar(1).phot_thresh
      mphot   = vpar(1).max_photons
      mtime   = vpar(1).max_time
*      -------------
*      get the hits
*      -------------
      do ihit = 1, gvhit_h.nok 
*    
           vpd_x         = gvhit(1).x(1)
           vpd_y         = gvhit(1).x(2)
           vpd_z         = gvhit(1).x(3)
           vpd_Px        = gvhit(1).p(1)
           vpd_Py        = gvhit(1).p(2)
           vpd_Pz        = gvhit(1).p(3)
           vpd_e         = gvhit(1).e
           vpd_T_length  = gvhit(1).ds
           vpd_Tof       = gvhit(1).tof
           vpd_dE        = gvhit(1).de
           vpd_numbv     = gvhit(1).volume_id
*    
*          ---------------
*          get pmt labels
*          ---------------
*    
           If(vpd_numbv.lt.2000)then
                vpd_iudet = vpd_numbv - 1000
                vpd_numbv = 1
           else
                vpd_iudet = vpd_numbv - 2000
                vpd_numbv = 2
           endif
           If(vpd_iudet.lt.200)then
                div_number = vpd_iudet - 100
                vpd_iudet = 1
           else
                div_number = vpd_iudet - 200
                vpd_iudet = 2
           endif
           ipmt = (vpd_numbv - 1)*npmt/2 + 
     &            8*(vpd_iudet - 1) + div_number
    
*          -----------------------
*          get photon info
*          -----------------------
*    
           p = SQRT(vpd_Px**2 + vpd_Py**2 + vpd_Pz**2)
     
           if (vpd_e .ne. 0 .and. vpd_e .lt. 2.) then
               beta = p/vpd_e
           else 
               beta = 0.
           endif
     
           If(beta.gt.1/ref_in) then
              c_theta = ACOS(1/(beta*ref_in))
*    
              n_photons  = kn*vpd_T_length*(SIN(c_theta))**2
           
              tot_photons(ipmt) = tot_photons(ipmt) + n_photons

              if (n_photons .gt. pthresh ) then
*    
*          ----------------------
*          get time information
*          ----------------------
*    
                 CALL RANNOR(random1,random2)
*    
                 tim(ipmt)  = vpd_Tof + t_rad/clt*
     &                       ref_in + 
     &                       t_res*random1
*    
*          ----------------------------------
*          Large time will be equal to 1 here
*          ----------------------------------
*    
                 tim(ipmt)  = min(1.,tim(ipmt))
                 vpd_tim(ipmt) = min(vpd_tim(ipmt),tim(ipmt))
                 If(vpd_tim(ipmt).eq.0.)vpd_tim(ipmt) = tim(ipmt)
*
               endif
            endif
*    
      end do
*    
*    
      vsim_h.nok = 0
      vraw_h.nok = 0
*    
*     ------------
*     fill table
*     ------------
*       

      do ipmt = 1, npmt
*    
        vsim_h.nok            = vsim_h.nok + 1
        vsim(1).photons(ipmt) = tot_photons(ipmt)
        vsim(1).time(ipmt)    = vpd_tim(ipmt)
*
        if( tot_photons(ipmt) .gt. 0 ) then
           vraw_h.nok                    = vraw_h.nok + 1
           vraw(vraw_h.nok).id           = vraw_h.nok 
           vraw(vraw_h.nok).pmt          = ipmt
           vraw(vraw_h.nok).vpd_adc      = 
     &                             arang*tot_photons(ipmt)/mphot
           vraw(vraw_h.nok).vpd_tdc      = trang*vpd_tim(ipmt)/mtime
*    
        endif
        tot_photons(ipmt) = 0
        vpd_tim(ipmt)     = 0
      end do
*    
*    
*
      VPS=STAFCV_OK
999   RETURN
      END

C=======================================================================
