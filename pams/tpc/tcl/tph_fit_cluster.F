* $Id: tph_fit_cluster.F,v 1.1 1998/01/27 00:49:15 fisyak Exp $
* $Log: tph_fit_cluster.F,v $
* Revision 1.1  1998/01/27 00:49:15  fisyak
* Split sources
*
      Subroutine tph_fit_cluster(tclpar, nseq, nclspix, npix_seq,
     +     ipad, itdc, iadc, itrk,
     +     white_noise,  filtered_noise, nhits,
     +     max_hits_in_cluster,
     +     qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +     irow, isec, npads, ipad_lo, ipad_hi, itdc_lo, itdc_hi)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tclpar:         tcl switch table
C     nseq:           number of sequences in cluster
C     nclspix:        number of pixels in cluster
C     npix_seq:       number of pixels in each sequence in cluster
C     ipad:           for each pixel, what is pad #
C     itdc:           for each pixel, what is bucket #
C     iadc:           for each pixel, what is adc value
C     itrk:           for each pixel, what MC track contributed most
C     white_noise:    SCA and digitization noise
C     filtered_noise: Noise from SAS chip, after shaping
C     max_hits_in_cluster: max # hits allowed in an overlapping pattern
C     irow:           Row number for cluster
C     isec:           Sector number for cluster
C     npads:          Number of pads in row
C     ipad_lo:        Lower limit for pad number in cluster
C     ipad_hi:        Upper limit for pad number in cluster
C     itdc_lo:        Lower limit for time bucket in cluster
C     itdc_hi:        Upper limit for time bucket in cluster
C
C   Output arguments : 
C
C     nhits     - estimated number of hits in this cluster
C     qq        - "integrated charge" (sum of adc counts)
C     x,dx,prf  - position, uncertainty, and width along row (units=pads)
C     z,dz,zrf  - position, uncertainty, and width along time (units=buckets)
C     phi       - "orientation" of cluster
C     track     - MC track contributing the most pixels
C     mu1       - 1st moment of hit (mean)
C     mu2       - 2nd moment of hit (rms)
C     mu3       - 3rd moment of hit (skewness)
C     mu4       - 4th moment of hit (kurtosis)
C     mu5       - Number of pads for hit
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C	This subroutine replaces what had been a messy set of routine
C	calls in the main code.  It should help in fitting in a more
C	truly 2-dimentional way.
C	The pad direction is fit with a Gaussian parametrization.
C	The time direction is fit with a form reflecting real signals
C	measured in the lab.
C
C   Originally written 23 November 1994 by Mike Lisa
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modifications:
c   ==============
c   03-NOV-1997 --- R.Bossingham
c       * Add number of pads per row
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
c
c passed variables:
c
#include "PAM.inc"
#include "tcl_tclpar.inc"
        RECORD      /TABLE_HEAD_ST/                tclpar_h
        RECORD      /TCL_TCLPAR_ST/               tclpar(*)

        integer iadc(*)               ! ADC's for cluster pixels
        integer ipad(*)               ! Pads for cluster pixels
        integer ipad_lo, ipad_hi      ! Pad number extent for cluster
        integer irow                  ! Row number for cluster (1-45)
        integer isec                  ! Sector number for cluster (1-24)
        integer itdc(*)               ! Time buckets for cluster pixels
        integer itdc_lo, itdc_hi      ! Time bucket extent for cluster
        integer itrk(*)               ! Track ID for MC data
        integer max_hits_in_cluster   ! Max. number of hits per cluster
        integer nclspix               ! Number of pixels in cluster
        integer nhits                 ! Number of hits in cluster
        integer npads                 ! Number of pads in cluster's pad row
	integer npix_seq(*)           ! Number of pixels in sequence on pad
        integer nseq                  ! Number of sequences in cluster

        real dx(*)                    ! Uncertainty in x
        real dz(*)                    ! Uncertainty in z
        real filtered_noise           ! Noise from SAS chip, after shaping
        real mu1(*)                   ! 1st moment of hit (mean)
        real mu2(*)                   ! 2nd moment of hit (rms)
        real mu3(*)                   ! 3rd moment of hit (skewness)
        real mu4(*)                   ! 4th moment of hit (kurtosis)
        real mu5(*)                   ! Number of pads for hit
        real phi(*)                   ! Orientation of cluster
        real prf(*)                   ! Width along x
        real qq(*)                    ! Sum of ADC counts
        real track(*)                 ! MC track contributing most
        real white_noise              ! SCA and digitization noise
        real x(*)                     ! Position along row (pad units)
        real z(*)                     ! Position along drift (bucket units)
        real zrf(*)                   ! Width along z
c-----------------------------------------------------------------------
c local variables:
c
c*** skip the definition for the neural net too..... RRB: 03-NOV-1997
c    integer nhits_nnet        ! neural net estimator function
                                ! of #hits in cluster

        logical look_for_many         ! Look for mult. hits in cluster
c----------------------------------------------------------------
c functions:
c
        logical triage_cuts
c----------------------------------------------------------------

c*** skip the neural net for now..... mal 21jun95
***        nhits = nhits_nnet(nseq,nclspix,npix_seq,ipad,itdc,iadc)
	! this is experimental

      nhits=0
      if (tclpar(1).decon.ne.0) then
         if (tclpar(1).triage.eq.1) then
            look_for_many = triage_cuts(ipad,itdc,iadc,nclspix,tclpar)
         else
            look_for_many = .true.
         endif
         if (look_for_many) then
            call mountain_finder(tclpar, nseq, nclspix, npix_seq,
     +           ipad, itdc, iadc, itrk,
     +           white_noise, filtered_noise,nhits, qq, x, dx, prf,
     +           z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5, npads,
     +           irow, isec, ipad_lo, ipad_hi, itdc_lo, itdc_hi)
c ??? Direct write is forbidden
            if (nhits.gt.max_hits_in_cluster) 
     +           write(6,*)'TPHAM - ERROR! array overrun! - fix this!'
         endif
         if (nhits.eq.0) then
            call tph_fit_isolated_cluster(tclpar, nclspix,
     +           ipad, itdc, iadc, itrk,
     +           white_noise, filtered_noise,
     +           qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +           npads, irow, isec, ipad_lo, ipad_hi, itdc_lo, itdc_hi)
            nhits=1
         endif
c
c remove the following after done using it--
c  just useful for outputing triage parameter for training
c*****************
         if (tclpar(1).decon.eq.-1)
     +        call ouput_practice(ipad,itdc,iadc,nclspix,irow,nhits)
c*****************
      else
c 30aug97 - bug introduced by UW folks - must include the "mu's" in ALL
c  calls to tph_fit_isolated_cluster!! - fixed mal
         call tph_fit_isolated_cluster(tclpar, nclspix,
     +        ipad, itdc, iadc, itrk,
     +        white_noise, filtered_noise,
     +        qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +        npads, irow, isec, ipad_lo, ipad_hi, itdc_lo, itdc_hi)
         nhits=1
      endif
      
      return
      end
