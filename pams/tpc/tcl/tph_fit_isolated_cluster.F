* $Id: tph_fit_isolated_cluster.F,v 1.6 1998/08/26 20:34:21 hardtke Exp $
* $Log: tph_fit_isolated_cluster.F,v $
* Revision 1.6  1998/08/26 20:34:21  hardtke
* set clusters of 0 width to 1/sqrt(12)
*
* Revision 1.5  1998/08/25 16:50:00  sakrejda
* z_vector protexted agains overflows
*
* Revision 1.4  1998/08/20 21:29:48  sakrejda
* printing replaced by a message call
*
* Revision 1.3  1998/03/17 16:57:18  sakrejda
* temporary protection from NaN - RB is preparing a better one
*
* Revision 1.2  1998/02/17 18:11:04  love
* Repaired z position calculation
*
* Revision 1.1  1998/01/27 00:49:16  fisyak
* Split sources
*
      Subroutine tph_fit_isolated_cluster(tclpar, nclspix,
     +     ipad, itdc, iadc, itrk,
     +     white_noise, filtered_noise,
     +     qq,x,dx,prf,z,dz,zrf,phi,track,
     +     mu1,mu2,mu3,mu4,mu5,
     +     npads, irow, isec, ipad_lo, ipad_hi, itdc_lo, itdc_hi)

C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tclpar:         switch table for tcl
C     nclspix:        number of pixels in cluster
C     ipad:	      for each pixel, what is pad #
C     itdc:	      for each pixel, what is bucket #
C     iadc:	      for each pixel, what is adc value
C     itrk:           for each pixel, what MC track contributed most
C     white_noise:    SCA and digitization noise
C     filtered_noise: Noise from SAS chip, after shaping
C     npads:          number of pads in row
C     irow:           Row number for cluster
C     isec:           Sector number for cluster
C     ipad_lo:        pad number lower limit for cluster
C     ipad_hi:        pad number upper limit for cluster
C     itdc_lo:        TDC lower limit (1-512) for cluster
C     itdc_hi:        TDC upper limit (1-512) for cluster
C
C   Output arguments : 
C
C     qq         - "integrated charge" (sum of adc counts)
C     x,dx,prf  - position, uncertainty, and width along row (units=pads)
C     z,dz,zrf  - position, uncertainty, and width along time (units=buckets)
C               - as of 22Dec97 units are cm and z's are real?
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
c       * Use lower and upper tdc, pad-number limits to speed code;
c         change arguments accordingly
c       * Update max. pads in row -> 182, max. time buckets -> 512.
c       * Replace most calls to vzero with in-line code (for speed)
c       * Replace calls to vfloat with in-line code (for speed)
c       * Add number of pads in row to calling arguments
c       * Add comments
C
c   11-NOV-1997 --- R.Bossingham
c       * Change pad_proj, numpix_pad_proj arrays from integer to real
c       * Change time_proj, numpix_time_proj arrays from integer to real
c       * Change tdc_x_adc array from integer to real
C
C   13-Feb-1998 --- Bill Love
C       * Around Xmas I messed up the conversion of the z fit to work 
C         with a real z value for each pixel somewhat - try to fix it 
C         today.
C
C   26-Aug-1998 --- D. Hardtke
C       * For zero width clusters, set prf (zrf) to 1/sqrt(12) times intrinsic
C         width.  Of course, these really shouldn't exist!
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
c passed inputs
c
#include "PAM.inc"
#include "tcl_tclpar.inc"
      RECORD      /TABLE_HEAD_ST/                tclpar_h
      RECORD      /TCL_TCLPAR_ST/               tclpar(*)

        integer iadc(*)               ! ADC's for cluster pixels
        integer ipad(*)               ! Pads for cluster pixels
        integer ipad_lo, ipad_hi      ! Pad number extent for cluster
        integer itdc(*)               ! Time buckets for cluster pixels
        integer itdc_lo, itdc_hi      ! Time bucket extent for cluster
        integer itrk(*)               ! Track ID for MC data
        integer nclspix               ! Number of pixels in cluster
        integer npads                 ! Number of pads in cluster's pad row
        integer irow                  ! Row number for cluster (1-45)
        integer isec                  ! Sector number for cluster (1-24)

c passed outputs
	real centroid                 ! Centroid of hit along pad row
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
	real sigma                    ! Width of hit
        real track(*)                 ! MC track contributing most
	real uncertainty              ! Uncertainty in centroid
        real white_noise              ! SCA and digitization noise
        real x(*)                     ! Position along row (pad units)
        real z(*)                     ! Position along drift (bucket units)
        real zrf(*)                   ! Width along z
C-----------------------------------------------------------------------
c local variables:
c 
        integer nbuck_max             ! Total number of time buckets
        integer nmc_tracks_max        ! Max. number of MC tracks
        integer npads_max             ! Largest number of pads in any row
c
        parameter(nbuck_max=512)
        parameter(nmc_tracks_max=2000)
        parameter(npads_max=182)
c
        integer id                    ! Message ID
        integer imax                  ! Index for local maxima found
        integer imc_track(nmc_tracks_max)   ! List of MC track ID's
        integer indx                  ! General index
        integer ipad_ipix             ! Pad for pixel
        integer ipad_range            ! Number of pads in cluster
        integer ipix                  ! Cluster pixel index
        integer ir                    ! Index of cluster moments
        integer iret                  ! general return status indication
        integer itdc_ipix             ! Time bucket for pixel
        integer ktrk                  ! Temp. MC track number 
        integer mtrk                  ! Index for nocc_track array
        integer nmc_tracks            ! Number of MC tracks
        integer nocc_track(nmc_tracks_max)  ! Number of pixels on MC track
        integer pad_n                 ! Pad index
c
        real adc_ipix                 ! ADC for pixel
        real adc_sum                  ! ADC sum in cluster
        real b                        ! Intercept of linear fit
        real badness                  ! "Badness" of linear fit
        real d_rvec(nbuck_max)        ! temp array
        real d_rvec_cor(nbuck_max)    ! SCA noise
        real d_rvec_uncor(nbuck_max)  ! SAS noise
        real diff(4)                  ! Temp. values for effic.
        real identity_vector(npads_max)  ! useful for x-axis of proj.
        real numpix_pad_proj(npads_max)  ! Sum of pixels used in proj.
        real z_vector(nbuck_max)      ! Z's for every time bin in cluster.
        real zdummy                   ! Local debug aid
        real pad_proj(npads_max)      ! Pad projection of cluster
        real rmu(4)                   ! Cluster moments
        real rvec(nbuck_max)          ! temp array
        real slope                    ! Slope of linear fit
        real tdc_x_adc(npads_max)     ! Sum of tdc*adc of cluster
        real bucket_length_in         ! length (cm) of inner sector time bucket
        real bucket_length_out        ! length (cm) of outer sector time bucket
c
        logical first                 ! .T. iff first call
        logical lookat_mc             ! Look at Monte Carlo info.

C-----------------------------------------------------------------------
c functions:
c
        integer iucomp
        integer lvsimx
        integer tgc_time_to_z
        integer loop_over_z
C        integer tph_3point_gauss   ! not in use?
C-----------------------------------------------------------------------
        data first/.true./
        data imc_track /nmc_tracks_max*0/
        data nocc_track /nmc_tracks_max*0/
C-----------------------------------------------------------------------
        save first
        save identity_vector
        save imc_track
        save lookat_mc
        save nocc_track
        save bucket_length_in
        save bucket_length_out
C-----------------------------------------------------------------------

c set up the identity vector for x (pad) calculation
        if (first) then
           first = .false.
           do indx=1,npads_max
              identity_vector(indx) = float(indx)
           enddo
           lookat_mc = (tclpar(1).mc.eq.1)
c get difference in z-positions for two randomly selected pixels:
           iret=tgc_time_to_z(50,50,5,5,zdummy) 
           if(iret.eq.1)bucket_length_in = zdummy
           iret=tgc_time_to_z(51,50,5,5,zdummy) 
           if(iret.eq.1)bucket_length_in = zdummy - bucket_length_in
           iret=tgc_time_to_z(50,50,35,5,zdummy) 
           if(iret.eq.1)bucket_length_out = zdummy
           iret=tgc_time_to_z(51,50,35,5,zdummy) 
           if(iret.eq.1)bucket_length_out = zdummy - bucket_length_out
        endif

c loop over all pixels, in the process:
c 1) sum all adc counts == q
c 2) project onto padrow (will fit projection to find x,dx,prf)
c 3) convert time values of all pixels to z coordinates.
c 4) get time average for the cluster (and obtain orientation)
c 5) sum up adc contribs from all MC tracks (will check to see who gave most)

        ipad_range = ipad_hi - ipad_lo + 1
c
        do indx=ipad_lo,ipad_hi
           pad_proj(indx) = 0.
           tdc_x_adc(indx) = 0.
           numpix_pad_proj(indx) = 0.
        end do
c
        adc_sum = 0.
        do ipix=1,nclspix
           adc_ipix = FLOAT(iadc(ipix))
           adc_sum = adc_sum + adc_ipix
           ipad_ipix = ipad(ipix)
           itdc_ipix = itdc(ipix)

C pad direction projection
           pad_proj(ipad_ipix)  = pad_proj(ipad_ipix) + adc_ipix
           tdc_x_adc(ipad_ipix) = tdc_x_adc(ipad_ipix)
     +                          + FLOAT(itdc_ipix)*adc_ipix
           numpix_pad_proj(ipad_ipix)  = numpix_pad_proj(ipad_ipix)+1.
C time direction no longer a projection - WAL 13 Feb 98
           if(ipix.le.nbuck_max) then
             iret=tgc_time_to_z(itdc_ipix,ipad_ipix,irow,isec,zdummy) 
             z_vector(ipix) = 0.0
             if(iret.eq.1)z_vector(ipix) = zdummy
           endif
        enddo
	if (adc_sum.eq.0.) stop 'Hey -- ADCSUM=0!!!!'

c mal 30aug97 - maybe a problem with decon=0 running - does this 
c screw up for very wide clusters??????????
C------------------------------------------------------------------
C     Added by DSW on July 6, 1997 to study the pad resolution shootup
C     at large pad crossing angles.
	do ir=1,4
           rmu(ir) = 0.
        enddo
C
        do pad_n=ipad_lo, ipad_hi
           rmu(1)  =  rmu(1) + pad_proj(pad_n)*pad_n
        enddo
        rmu(1) =  rmu(1)/adc_sum

C     Calculate  sigma, skewness & kurtosis;
C RRB: Speed calc. by reordering loop and carrying previous results forward:
        do pad_n=ipad_lo, ipad_hi
           diff(1) = float(pad_n) - rmu(1)
           do ir = 2,4
              diff(ir) = diff(1)*diff(ir-1)
              rmu(ir) =  rmu(ir) + pad_proj(pad_n)*diff(ir)
           enddo
        enddo
C
        do ir = 2,4
           rmu(ir) = rmu(ir)/adc_sum
        enddo

        mu1(1) = rmu(1)
        mu2(1) = sqrt(rmu(2))
        mu3(1) = rmu(3)/rmu(2)**1.5
        mu4(1) = rmu(4)/rmu(2)**2 - 3.
        mu5(1) = float(ipad_range)
C------------------------------------------------------------------
c mal 30aug97 - just put the damn things to zero-- they seem to be randomly
c assigned if not!!
c	mu1(1) = 0.0
c	mu2(1) = 0.0
c	mu3(1) = 0.0
c	mu4(1) = 0.0
c	mu5(1) = 0.0

c     q:
        qq(1) = adc_sum

c x,dx,prf: - try 3-point gauss... if does not work, do weighted mean
        do indx=ipad_lo,ipad_hi
           rvec(indx) = pad_proj(indx)
           if (pad_proj(indx).ne.0.) then
              d_rvec(indx) = numpix_pad_proj(indx)
     +             * sqrt(filtered_noise**2+white_noise**2)
           else
              d_rvec(indx)=0.0
           endif
           d_rvec_cor(indx) = 0.0
        enddo
cmal 30aug97        if (tph_3point_gauss(identity_vector, rvec, d_rvec,
cmal/rrb     +       ipad_lo, ipad_hi, npads,
cmal/rrb     +       centroid, sigma, uncertainty).ne.1) 
cmal     +       call tph_weighted_mean(identity_vector, rvec,
        call tph_weighted_mean(identity_vector, rvec,
     +       d_rvec_cor, d_rvec,
     +       ipad_lo, ipad_hi, npads,
     +       centroid, sigma, uncertainty)
        x(1) = centroid
        dx(1) = uncertainty
        if (sigma.gt.0.) then
         prf(1) = sigma
        else
         prf(1) = 1/sqrt(12.)
        endif

c z,dz,zrf: - do weighted mean
c  for uncertainties, see notebook2 p113
c
        loop_over_z=nclspix
        if(loop_over_z.gt. nbuck_max)
     +  loop_over_z=nbuck_max
        do indx=1,loop_over_z
           rvec(indx) = float(iadc(indx))
           d_rvec_cor(indx) = filtered_noise
           d_rvec_uncor(indx) = white_noise
        enddo
        call tph_weighted_mean(z_vector, rvec,
     +       d_rvec_cor, d_rvec_uncor,
     +       1, loop_over_z, nbuck_max,
     +       centroid, sigma, uncertainty)
c
        z(1) = centroid
        dz(1) = uncertainty
        if (sigma.gt.0.) then
         zrf(1) = sigma
        else if (irow.gt.13) then
         zrf(1) = bucket_length_out/sqrt(12.)
        else 
         zrf(1) = bucket_length_in/sqrt(12.)
        endif

c orientation phi:
c ??? If pad_proj *is* 0, rvec is peculiar...but I left existing *logic*
        do indx=ipad_lo,ipad_hi
           d_rvec(indx) = pad_proj(indx)
           if (pad_proj(indx).ne.0.) then
              rvec(indx) = tdc_x_adc(indx)/d_rvec(indx)
           else
              rvec(indx) = tdc_x_adc(indx)
           end if
        enddo
        if(ipad_range.lt.3) then
        phi(1)=-1.0
        else
	call lfitw(identity_vector(ipad_lo), rvec(ipad_lo),
     +             d_rvec(ipad_lo), ipad_range, 0,
     +             slope, b, badness)
        phi(1) = slope
        endif

c track: - who contributed the most pixels (MC data)
c this following is only useful for MC data-- turn it off with 
c  switch if interested in real data (or real data performance)
        if (lookat_mc) then
c
c RRB: Replace this, initializing, then clearing the used space afterwards:
c          call vzero(imc_track,nmc_tracks_max)
c          call vzero(nocc_track,nmc_tracks_max)
           nmc_tracks = 0
           do ipix=1,nclspix
              ktrk=itrk(ipix)
              mtrk=iucomp(ktrk,imc_track,nmc_tracks)
              if (mtrk.eq.0) then
                 if (nmc_tracks.lt.nmc_tracks_max) then
                    nmc_tracks=nmc_tracks+1
                    imc_track(nmc_tracks) = ktrk
                    nocc_track(nmc_tracks) = 1
                 else           ! too many contributing tracks... forget it
                    id=0
*                    call message('TPHAM-I-2MANY, ntrk.gt.max',1,id)
                     write(6,*) 'TPHAM-I-2MANY, ntrk.gt.max'
                 endif
              else
                 nocc_track(mtrk) = nocc_track(mtrk)+1
              endif
           enddo
           imax = lvsimx(nocc_track,nmc_tracks,1)
           track(1) = imc_track(imax)
c
c Now, clean up the workspace for next time:
           call vzero(imc_track,nmc_tracks)
           call vzero(nocc_track,nmc_tracks)
c
        endif

        return
        end
