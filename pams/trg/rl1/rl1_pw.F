CC:>--------------------------------------------------------------------
CC: FILE:      rl1_pw.F.template
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION RL1_PW(
     1            lvl0_h,                 lvl0 ,
     2            clb_h ,                 clb  ,
     3            l1b_h ,                 l1b  ) 
      IMPLICIT NONE
#include "rl1_pw.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    rl1_pw
CC: DESCRIPTION: Offline simulation of autocorrelation analysis for 
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
CC:     clbh:       level 1 input control header - autocorrelation analysis
CC:     clb:        level 1 input control - autocorrelation analysis
CC:     l1bh:       level 1 output header - autocorrelation analysis
CC:     l1b:        level 1 output - autocorrelation analysis
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
*   
*
        real    amp(100) 
        real    b(100), c(100), s(100), bb(100), ab(100)        
	real    d1(100), d2(100), d3(100)
	real    psum, ppsum, phsum, ph(100), p(100)
        integer number_bins, nmwc_eta, nmwc_phi, nctb_eta, nctb_phi
        integer band_width, nul_hyp
        real    ss(100), cc(100), ctb_factor, mwc_factor
        real    sum1, sum2, f2bar, fbar2, sig2, ff
        integer ijk, kmwc, kctb
        integer ai, aj, al, am, l, m
        real    first, auto, power, ri
*   
C       Executable Statements:
C========================================================================
*
        l1b_h.nok = 1
*
        rl1_pw = STAFCV_OK
*
*       ------------------------------------------------------------------------     
*       If the number of bins and threshold are not set by the user, use default
*       values, assuming level 0 is passing an array that is 8x4 (eta x phi) and
*       that it will be compressed to 8 one dimensional bins.
*       Threshold value will probably need empirical adjusting.
*       ------------------------------------------------------------------------
*       
        if( clb(1).n_bins    .eq. 0   )     clb(1).n_bins     = 8
        if( clb(1).mwc_eta   .eq. 0   )     clb(1).mwc_eta    = 2
        if( clb(1).mwc_phi   .eq. 0   )     clb(1).mwc_phi    = 4
        if( clb(1).ctb_eta   .eq. 0   )     clb(1).ctb_eta    = 4
        if( clb(1).ctb_phi   .eq. 0   )     clb(1).ctb_phi    = 4
        if( clb(1).time_loop .eq. 0   )     clb(1).time_loop  = 1
        if( clb(1).bnd_width .eq. 0   )     clb(1).bnd_width  = 6
        if( clb(1).ctb_fact  .eq. 0   )     clb(1).ctb_fact   = 1.0
        if( clb(1).mwc_fact  .eq. 0   )     clb(1).mwc_fact   = 1.0
        if( clb(1).nul_hyp   .eq. 0   )     then
             do i = 1, clb(1).bnd_width
                    clb(1).s_coef(i)  = 0
                    clb(1).c_coef(i)  = 0
             end do
        endif       
*                 
        number_bins = clb(1).n_bins
        nmwc_eta    = clb(1).mwc_eta
        nmwc_phi    = clb(1).mwc_phi
        nctb_eta    = clb(1).ctb_eta
        nctb_phi    = clb(1).ctb_phi        
        band_width  = clb(1).bnd_width
        ctb_factor  = clb(1).ctb_fact
        mwc_factor  = clb(1).mwc_fact
        nul_hyp     = clb(1).nul_hyp
        do i = 1, band_width
               ss(i) = clb(1).s_coef(i)
               cc(i) = clb(1).c_coef(i)
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
*                         
             amp(i) = 0.
*                           
             if ( i .le. nmwc_eta ) then
*                           
                    do j = 1, nmwc_phi
                         kmwc   = kmwc + 1                 
                         amp(i) = amp(i) + float( lvl0(1).fpga1_mwc(kmwc) )
                    end do
*                           
             elseif (i .gt. nmwc_eta .and. i .le. ( nmwc_eta + nctb_eta ) ) then
*                             
                    do j = 1, nctb_phi
                         kctb   = kctb + 1    
                         amp(i) = amp(i) + float( lvl0(1).fpga1_ctb(kctb) )
                    end do
*                   
*                            
                    amp(i) = amp(i) / ctb_factor
*                        
             else
*                   
                    do j = 1, nmwc_phi
                         kmwc   = kmwc + 1           
                         amp(i) = amp(i) + float( lvl0(1).fpga1_mwc(kmwc) )
                    end do
             endif                       
        end do
*
*        CALL TIMED(first)
        do ijk = 1, clb(1).time_loop
*       
*-------------------------------------------------
*       autocorrelation into a
*	get mean square and square of mean of amp
*-------------------------------------------------      
*
        sum1 = 0.
*           
	do i = 1, number_bins
	     sum1 = sum1 + amp(i)
        end do
*
        sum1  = sum1 / float( number_bins )
        fbar2 = sum1 * sum1
*
*--------------------------------------------         
*	get sigma and mean product with error
*--------------------------------------------         
*
        sig2  = sum1
        ff    = (fbar2 - sig2 / float( number_bins )) / fbar2
*
*-----------------------------------------          
*       get finite domain autocorrelation
*-----------------------------------------
*           
        do m = 1, number_bins
             am = m
             b(m) = 0.
             l    = number_bins - m + 1
*               
             do i = 1, l
                  k    = i + m - 1
                  b(m) = b(m) + amp(i) * amp(k) 
             end do
*              
             al       = l
             b(m)     = b(m) / ( al * fbar2) - ff
*                   
*---------------------------------------------------
*           convert for %
*---------------------------------------------------
*                  
             b(m)     = 100 * b(m) * al / float( number_bins )
*            
*------------------------------------
*  check if null hypothesis selected  
*------------------------------------
*            
             if( nul_hyp .eq. 1 ) then
*                     
*---------------------------------------------------   
* form null hypothesis from  Fourier coefficients
*---------------------------------------------------
*                   
*                    
                 bb(m)=0
*                  
                 do  k = 1, band_width
*                     
                     bb(m) = bb(m) + ss(k) * sin( 2 * 3.14159 * k * am / float( number_bins ) )
                     bb(m) = bb(m) + cc(k) * cos( 2 * 3.14159 * k * am / float( number_bins ) )
*                   
                 end do
*                                  
*--------------------------   
*  subract null hypothesis
*--------------------------
*                  
                 ab(m) = b(m)
                 b (m) = b(m) - bb(m)  
*                  
             endif
        end do   
*                	
*        b(1) = b(1) - sig2
        b(1) = b(1) - number_bins / sig2
*         
        end do
*        CALL TIMED(auto)
        do ijk = 1, clb(1).time_loop
*         	
*-------------------------------------------
*       Fourier transform of autocorrelation
*-------------------------------------------
*
        do i = 1, number_bins/2
             sum1 = 0
             sum2 = 0 
*
             do j = 1, number_bins             
                  ai   = i
                  aj   = j
                  sum1 = sum1 + sin( 2 * aj * 3.1415926 * ai / float( number_bins ) ) * b(j)
                  sum2 = sum2 + cos( 2 * aj * 3.1415926 * ai / float( number_bins ) ) * b(j)
             end do
*
*------------------------
*     Fourier spectra
*------------------------
*
             s(i) = 2 * sum1 / float( number_bins )
             c(i) = 2 * sum2 / float( number_bins )
*
*----------------------
*       power and phase
*----------------------
*
             p(i)  = ( s(i) * s(i) + c(i) * c(i) )  
             ph(i) = atan2 ( s(i), c(i) )
        end do
*
*------------------------------------------
*       total power p0 - bandwidth limited
*------------------------------------------
*
        psum = 0 
        do i = 1, band_width
             psum = psum + p(i)
        end do
*
*------------------------
*       second moment p2
*------------------------
*
        ppsum = 0
        do i = 1, number_bins / 2
             ppsum = ppsum + p(i) * i * i 
        end do
        ppsum = ppsum / psum
*
*--------------------------
*       weighted phase
*--------------------------
*
        do i = 1, number_bins / 2
             ph(i) = ph(i) * p(i) / ( psum + .1 )
        end do
*
*----------------------
*       total phase
*----------------------
*
        phsum = 0 
        do i = 1, band_width
             phsum = phsum + ph(i)
        end do
*       
*------------------------------------------
*  maintain fixed minimum vertical scales
*------------------------------------------
*
        p (number_bins / 2) = 10
        ph(number_bins / 2) = .2
*
        end do
*        CALL TIMED(power)

*        
*       -------------------------------------------------------
*       Fill tables. First autocorrelation, then power spectra.
*       -------------------------------------------------------
*
        do i = 1, number_bins
               l1b(1).autonull(i) = b(i)
               l1b(1).autocorr(i) = ab(i)
        end do
*
*----------------------------------
*    Fourier spectra coefficients
*----------------------------------
*
        do i = 1, band_width
            l1b(1).s1(i) = s(i)
            l1b(1).c1(i) = c(i)
        end do
*
        l1b(1).power      = psum
        l1b(1).phase      = phsum
*
*
        return
        end