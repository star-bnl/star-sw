CC:>--------------------------------------------------------------------
CC: FILE:      rl1_ks.F.template
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION RL1_KS(
     1            lvl0_h,                 lvl0 ,
     2            cla_h ,                 cla  ,
     3            l1a_h ,                 l1a  ) 
      IMPLICIT NONE
#include "rl1_ks.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    RL1_KS
CC: DESCRIPTION: Offline simulation of k-s test analysis for 
CC:              trigger level 1.    
CC:        
CC:        
CC: 
CC: 
CC: AUTHOR:     zm  - Z. Milosevich, milosevich@cmchem.chem.cmu.edu
CC: ARGUMENTS:
CC:          IN:
CC:       INOUT:
CC:     lvl0h:      level 0 output header
CC:     lvl0:       level 0 output
CC:     clah:       level 1 input control header - k-s test
CC:     cla:        level 1 input control - k-s test
CC:     l1ah:       level 1 output header - k-s test
CC:     l1a:        level 1 output - k-s test
CC:         OUT:
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

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
        l1a_h.nok = 1
*
        RL1_KS = STAFCV_OK
*
*       ------------------------------------------------------------------------     
*       If the number of bins and threshold are not set by the user, use default
*       values, assuming level 0 is passing an array that is 8x4 (eta x phi) and
*       that it will be compressed to 8 one dimensional bins.
*       Threshold value will probably need empirical adjusting.
*       ------------------------------------------------------------------------
*       
        if( cla(1).n_bins    .eq. 0   )     cla(1).n_bins     =  8
        if( cla(1).threshold .eq. 0.0 )     cla(1).threshold  =  1.6
        if( cla(1).mwc_eta   .eq. 0   )     cla(1).mwc_eta    =  2
        if( cla(1).mwc_phi   .eq. 0   )     cla(1).mwc_phi    =  4
        if( cla(1).ctb_eta   .eq. 0   )     cla(1).ctb_eta    =  4
        if( cla(1).ctb_phi   .eq. 0   )     cla(1).ctb_phi    =  4
*                 
        number_bins = cla(1).n_bins
        nmwc_eta    = cla(1).mwc_eta
        nmwc_phi    = cla(1).mwc_phi
        nctb_eta    = cla(1).ctb_eta
        nctb_phi    = cla(1).ctb_phi
        thresh      = cla(1).threshold
*
*       ------------------------------------------------------------------------        
*       If there is no user input for the reference data (ctrl.ref_data_flag=0)
*       the use default LUND values for Au + Au.
*       ------------------------------------------------------------------------
*
        if( cla(1).ref_data_flag .eq. 0) then
*               
              do i = 1, number_bins
                  cla(1).ref_data(i) = 1
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
             prob(i)    = float( cla(1).ref_data(i) )
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
*        CALL TIMED(first)
*
      do j = 1, cla(1).time_loop
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
*       CALL TIMED(second)
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

       l1a(1).ks_d         = d
       l1a(1).ks_sig       = significance
       l1a(1).accept       = iflag


999   RETURN
      END
C=======================================================================
