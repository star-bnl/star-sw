****************************************************************
      INTEGER FUNCTION ks_1d_pdy( lvl0h, lvl0, ctrlh, ctrl, rl1h, rl1)
      IMPLICIT NONE
C   Input arguments  (One to a line with definition after ! ) 
C
C     binh:       bin information header
C     bin:        bin information
C     lvl0h:      level 0 output header
C     lvl0:       level 0 output
C
C   Output arguments : 
C     iok = user status code
C     
C   Functional Description : 
C     Paul DeYoung's 1-Dimensional K-S test code.
C   Created  14-Oct-1993   Zoran Milosevich
C   Copyright 1993    Lawrence Berkeley Laboratory
C   Error conditions : 
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
#include "rl0_rl0_data_pars.inc"
#include "rl0_rl0_data_st.inc"
#include "rl1_ks_ctrl_pars.inc"
#include "rl1_ks_ctrl_st.inc"
#include "rl1_ks_out_pars.inc"
#include "rl1_ks_out_st.inc"
C-----------------------------------------------------------------------
      RECORD / table_head_st   / lvl0h 
      RECORD / rl0_data_row_st / lvl0(*)
      RECORD / table_head_st   / ctrlh
      RECORD / ks_ctrl_row_st  / ctrl(*)
      RECORD / table_head_st   / rl1h
      RECORD / ks_out_row_st   / rl1(*)
C
      INTEGER nok
      INTEGER ierr

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       =====================
        INTEGER I,J,K
*
        real    x1(100), x2(100), prob(100)
        real    int_prob(100)
        integer number_bins, nmwc_eta, nmwc_phi, nctb_eta, nctb_phi
        real    thresh
        real    total_prob, total
        real    first, second
        real    d, fn, ff, en, significance
        integer iflag, kmwc, kctb
        logical flag
*   
C       Executable Statements:
C=======================================================================
C     
        rl1h.nok = 1
*
        ks_1d_pdy = tucod_ok
*
*       ------------------------------------------------------------------------     
*       If the number of bins and threshold are not set by the user, use default
*       values, assuming level 0 is passing an array that is 8x4 (eta x phi) and
*       that it will be compressed to 8 one dimensional bins.
*       Threshold value will probably need empirical adjusting.
*       ------------------------------------------------------------------------
*       
        if( ctrl(1).n_bins    .eq. 0   )     ctrl(1).n_bins     =  8
        if( ctrl(1).threshold .eq. 0.0 )     ctrl(1).threshold  =  1.6
        if( ctrl(1).mwc_eta   .eq. 0   )     ctrl(1).mwc_eta    =  2
        if( ctrl(1).mwc_phi   .eq. 0   )     ctrl(1).mwc_phi    =  4
        if( ctrl(1).ctb_eta   .eq. 0   )     ctrl(1).ctb_eta    =  4
        if( ctrl(1).ctb_phi   .eq. 0   )     ctrl(1).ctb_phi    =  4
*                 
        number_bins = ctrl(1).n_bins
        nmwc_eta    = ctrl(1).mwc_eta
        nmwc_phi    = ctrl(1).mwc_phi
        nctb_eta    = ctrl(1).ctb_eta
        nctb_phi    = ctrl(1).ctb_phi
        thresh      = ctrl(1).threshold
*
*       ------------------------------------------------------------------------        
*       If there is no user input for the reference data (ctrl.ref_data_flag=0)
*       the use default LUND values for Au + Au.
*       ------------------------------------------------------------------------
*
        if( ctrl(1).ref_data_flag .eq. 0) then
*               
              do i = 1, number_bins
                  ctrl(1).ref_data(i) = 1
              end do
*               
        endif        
*
*       ---------------------           
*       Normalize reference.
*       ---------------------
*
	total_prob = 0
*              
	do i = 1, number_bins
*                
             prob(i)    = float( ctrl(1).ref_data(i) )
             total_prob = total_prob + prob(i)
*                
        end do
*             
        int_prob(1) = prob(1) / total_prob
*        
        do i = 2, number_bins
             int_prob(i) = int_prob( i - 1 ) + ( prob(i) / total_prob )
        end do
*
*       ---------------------------------------------------------------------
*       Add all bins in phi for given eta value for one dimensional analysis.
*       ---------------------------------------------------------------------
*
        kmwc = 0
        kctb = 0
*
        do i = 1, number_bins
             x1(i) = 0
*                           
             if ( i .le. nmwc_eta ) then
*                           
                    do j = 1, nmwc_phi
                         kmwc  = kmwc + 1                 
                         x1(i) = x1(i) + float( lvl0(1).fpga1_mwc(kmwc) )
                    end do
*                           
             elseif (i .gt. nmwc_eta .and. i .le. ( nmwc_eta + nctb_eta ) ) then
*                             
                    do j = 1, nctb_phi
                         kctb  = kctb + 1   
                         x1(i) = x1(i) + float( lvl0(1).fpga1_ctb(kctb) )
                    end do
*                                          
             else
*                   
                    do j = 1, nmwc_phi
                         kmwc  = kmwc + 1           
                         x1(i) = x1(i) + float( lvl0(1).fpga1_mwc(kmwc) )
                    end do
             endif                       
        end do
*
        CALL TIMED(first)
*
      do j = 1, 100000
*         
        flag = .false.
*        
        total=0
*         
        do i = 1, number_bins
	     total = total + x1(i)
        end do
*       
        x2(1) = x1(1)
*
        do i = 2, number_bins
             x2(i) = x1(i) + x2( i - 1 )
        end do
*        
        d=0.
*      
        do i = 1, number_bins
             fn = x2(i) / total
             ff = int_prob(i)
             d = max( d, abs( fn - ff ) )
        end do
*       
        en = sqrt( total )
        if( ( en + 0.12 + 0.11/en )*d .gt. thresh ) flag = .true.
*            
      end do
*
       CALL TIMED(second)
*
       significance = ( en + 0.12 + 0.11/en )*d
*        
       if( flag ) then
          iflag = 1
       else
          iflag = 0
       endif 
*        
*      ------------
*      Fill tables.
*      ------------
*

       rl1(1).ks_d         = d
       rl1(1).ks_sig       = significance
       rl1(1).accept       = iflag
       rl1(1).cpu          = second

999   RETURN
      END
C=======================================================================
