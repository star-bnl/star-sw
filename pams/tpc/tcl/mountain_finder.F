* $Id: mountain_finder.F,v 1.2 1998/03/17 16:57:17 sakrejda Exp $
* $Log: mountain_finder.F,v $
* Revision 1.2  1998/03/17 16:57:17  sakrejda
* temporary protection from NaN - RB is preparing a better one
*
* Revision 1.1  1998/01/27 00:49:09  fisyak
* Split sources
*
      Subroutine mountain_finder(tclpar, nseq, nclspix, npix_seq,
     +     ipad, itdc, iadc, itrk,
     +     white_noise, filtered_noise,
     +     nhits,
     +     qq,x,dx,prf,z,dz,zrf,phi,track,mu1,mu2,mu3,mu4,mu5,
     +     npads, irow, isec, ipad_lo, ipad_hi, itdc_lo, itdc_hi)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tclpar:         switch table for tcl
C     nseq:           number of sequences in cluster
C     nclspix:        number of pixels in cluster
C     npix_seq:       number of pixels in each sequence in cluster
C     ipad:           for each pixel, what is pad #
C     itdc:	      for each pixel, what is bucket #
C     iadc:	      for each pixel, what is adc value
C     itrk:           for each pixel, what MC track contributed most
C     white_noise:    SCA and digitization noise
C     filtered_noise: Noise from SAS chip, after shaping
C     npads:          Number of pads in row
C     irow:           Row number for the cluster
C     isec:           Sector number for the cluster 
C     ipad_lo:        Lower limit for pad number in cluster
C     ipad_hi:        Upper limit for pad number in cluster
C     itdc_lo:        Lower limit for time bucket in cluster
C     itdc_hi:        Upper limit for time bucket in cluster
C
C   Output arguments : 
C
C     nhits     - number of peaks found
C     q         - "integrated charge" (sum of adc counts)
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
c
c   modifications:
c   ==============
c   03-NOV-1997 --- R.Bossingham
c       * Use lower and upper tdc, pad-number limits to speed code;
c         change arguments accordingly
c       * Update max. pads in a row -> 182, max. time buckets -> 512
c       * Change max. pad, max. bucket names for eventual globalization
c       * Add comments
c       * Allow reset of edges of "allowed" array
c       * Explicitly save the "allowed" array
c       * Replace most temp. arguments to tph_fit_isolated_cluster
c         with final array locations (for a bit of speed)
c
c   11-NOV-1997 --- R.Bossingham
c       * Split max_pos(2,nmax_max) into max_pad(nmax_max), max_tdc(nmax_max)
c       * Protect against overflows in temp. arrays for a hit
c       * Reverse more array indexing orders (for speed)
c
c   14-NOV-1997 --- R.Bossingham
c       * If multiple peaks: order from shortest to tallest, assign very
c         nearby pixels to each peak, then follow the slope of each peak
c         down until the elevation shows "significant" increase relative
c         to the previous pixel, or until one encounters pixels already
c         allocated to another peak.  No true deconvolution is attempted,
c         and not all pixels will necessarily be assigned to a peak; i.e.,
c         plenty of room for improvement, still.
c
c   02-DEC-1997 --- R.Bossingham
c       * Use a less rectilinear cluster deconvolution;
c         peaks are now divided into four sectors along "~45 deg. lines"
c         and a search along a nominal axis now essentially searches
c         an angular range to the extent that the hit fills it, within
c         these 90 deg. sectors.  This deconvolutes long, angled hits better.
c
c         Previously, I searched a fixed range in pad or tdc number, once
c         the search stopped in the adjacent directions.
c
C-----------------------------------------------------------------------
c passed variables
c
#include "PAM.inc"
#include "tcl_tclpar.inc"
        RECORD      /TABLE_HEAD_ST/                tclpar_h
        RECORD      /TCL_TCLPAR_ST/               tclpar(*)

        integer ii                    ! INDEX FOR THE EMERGENCY LOOP
*********
*********
*********
        integer iadc(*)               ! ADC's for cluster pixels
        integer ipad(*)               ! Pads for cluster pixels
        integer ipad_lo, ipad_hi      ! Pad number extent for cluster
        integer itdc(*)               ! Time buckets for cluster pixels
        integer itdc_lo, itdc_hi      ! Time bucket extent for cluster
        integer itrk(*)               ! Track ID for MC data
        integer nclspix               ! Number of pixels in cluster
        integer nhits                 ! Number of hits in cluster
        integer npads                 ! Number of pads in cluster's pad row
        integer irow                  ! Row number for cluster (1-45)
        integer isec                  ! Sector number for cluster (1-24)
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

C-----------------------------------------------------------------------
c local variables:
c
        integer h_tol                 ! Tolerance on monotonic ADC decrease
        integer nbuck_max             ! Total number of time buckets
        integer nclpix_max            ! Max. pixels in cluster
        integer nmax_max              ! maximum number of local maxima
        integer npads_max             ! Largest number of pads in any row
        integer n_allow               ! Most possible pixels in a cluster
c
        parameter (h_tol=3)
        parameter (nbuck_max=512)
        parameter (nclpix_max=10000)
        parameter (nmax_max=100)
        parameter (npads_max=182)
        parameter (n_allow=npads_max*nbuck_max)
c
        integer assign(npads_max,nbuck_max) ! Assigned peak for pixel
        integer dpad                  ! ADC difference between maxima
        integer dtdc                  ! TDC difference between maxima
        integer height(npads_max,nbuck_max)    ! ADC(pad,TDC) in cluster
        integer i                     ! General-purpose index
        integer i_minimum_height      ! Min. height of local maxima
        integer i_sprd                ! Index for peak spreading
        integer iadc_temp(nclpix_max) ! ADC values for hit
        integer iclosest_pad          ! Min. allowed pad separation of maxima
        integer iclosest_tdc          ! Min. allowed TDC separation of maxima
        integer id                    ! Message ID
        integer imax                  ! Index for local maxima found
        integer index_list(nclpix_max)! Sorted index list
        integer indx                  ! General index
        integer infl_pad              ! Pad distance auto-assigned to peak
        integer infl_tdc              ! TDC distance auto-assigned to peak
        integer ipad_lo_temp          ! Lowest pad for hit
        integer ipad_hi_temp          ! Highest pad for hit
        integer ipad_temp(nclpix_max) ! Pad values for hit
        integer itdc_lo_temp          ! Lowest TDC for hit
        integer itdc_hi_temp          ! Highest TDC for hit
        integer itdc_temp(nclpix_max) ! TDC values for hit
        integer itrk_temp(nclpix_max) ! Track numbers for hit
        integer iwriteout             ! Pixel index for output
        integer jpad                  ! Pad for pixel
        integer jpix                  ! Pixel index
        integer jtdc                  ! Time bucket for pixel
        integer kpad                  ! Pad index
        integer kpad_lo, kpad_hi      ! kpad limits
        integer kpad_0                ! Pad index for pixel closer to peak 
        integer kpad_lo_lotdc         ! Lower pad for lo tdc segment
        integer kpad_hi_lotdc         ! Upper pad for lo tdc segment
        integer kpad_lo_hitdc         ! Lower pad for hi tdc segment
        integer kpad_hi_hitdc         ! Upper pad for hi tdc segment
        integer kpad_lo_lotdc_t       ! Lower pad in hit for lo tdc segment
        integer kpad_hi_lotdc_t       ! Upper pad in hit for lo tdc segment
        integer kpad_lo_hitdc_t       ! Lower pad in hit for hi tdc segment
        integer kpad_hi_hitdc_t       ! Upper pad in hit for hi tdc segment
        integer ktdc                  ! Time bucket index
        integer ktdc_lo, ktdc_hi      ! ktdc limits
        integer ktdc_0                ! TDC index for pixel closer to peak 
        integer ktdc_lo_lopad         ! Lower tdc for lo pad segment
        integer ktdc_hi_lopad         ! Upper tdc for lo pad segment
        integer ktdc_lo_hipad         ! Lower tdc for hi pad segment
        integer ktdc_hi_hipad         ! Upper tdc for hi pad segment
        integer ktdc_lo_lopad_t       ! Lower tdc in hit for lo pad segment
        integer ktdc_hi_lopad_t       ! Upper tdc in hit for lo pad segment
        integer ktdc_lo_hipad_t       ! Lower tdc in hit for hi pad segment
        integer ktdc_hi_hipad_t       ! Upper tdc in hit for hi pad segment
        integer max_height(nmax_max)  ! Height of local maxima
        integer max_pad(nmax_max)     ! Pad positions of local maxima
        integer max_tdc(nmax_max)     ! TDC positions of local maxima
        integer nclspix_temp          ! Number of pixels in hit
        integer nmax                  ! Number of local maxima found
        integer peak_list(nmax_max)   ! Maxima index list sorted by peak height
        integer track_pix(npads_max,nbuck_max) ! Track ID for MC data
        integer valley                ! Minima along ridge connecting maxima

        real dt                       ! Change in TDC, relative to peak
        real dp                       ! Change in pad, relative to peak
        real peak_to_valley           ! ADC separation of peak and valley
        real slope                    ! Slope of ridge between maxima

        logical allowed(npads_max,nbuck_max) ! Candidate pixels for maxima
        logical ktdc_lo_sprd          ! Peak spreading about - TDC axis
        logical ktdc_hi_sprd          ! Peak spreading about + TDC axis
        logical kpad_lo_sprd          ! Peak spreading about - pad # axis
        logical kpad_hi_sprd          ! Peak spreading about + pad # axis
c
C-----------------------------------------------------------------------
        data allowed /n_allow*.true./
c
C-----------------------------------------------------------------------
        save allowed
c
C-----------------------------------------------------------------------

        i_minimum_height=tclpar(1).mf_min
        iclosest_pad=tclpar(1).mf_cpad
        iclosest_tdc=tclpar(1).mf_ctim
c
c Check that temporary arrays for hit are large enough to prevent
c overflows; otherwise, print msg. and protect ourselves as best we can:
	if ((2*iclosest_pad-1)*(2*iclosest_tdc-1).gt.nclpix_max) then
           id=0
*           call message('TPHAM-I-2MANY pix in temp hit',1,id)
           write(6,*) 'TPHAM-I-2MANY pix in temp hit'
           iclosest_pad = INT(SQRT(FLOAT(nclpix_max))+1.)/2
           iclosest_tdc = iclosest_pad
        end if
c
C Clear a rectangular area containing the cluster with the arrays
c height(1:npads_max,1:nbuck_max) and track_pix(1:npads_max,1:nbuck_max):
      do ktdc=itdc_lo,itdc_hi
         do kpad=ipad_lo,ipad_hi
            height(kpad,ktdc) = 0
            track_pix(kpad,ktdc) = 0
         enddo
      enddo

c Load the cluster:
      do jpix=1,nclspix
         height(ipad(jpix),itdc(jpix)) = iadc(jpix)
         track_pix(ipad(jpix),itdc(jpix)) = itrk(jpix)
      enddo

c list pixels in descending order of adc occupancy...
      call sortzv(iadc,index_list,nclspix,-1,10,0)

C find maxima...
c here, index_list are the indices into ipad,itdc,iadc, in decending
c     order, of the pixels with maximum occupancy.  So, for each pixel
c     that comes up, just check to see if that pixel is allowed.
c     If so, then it is a mountain-top.
c Each pixel that comes up on my ordered list should "lay waste" to
c     surrounding territory, so
c     that if I get a pixel in my ordered list which is allowed, then
c     it is a probably a local maximum-- the only way it is not is
c     when there is a pleteau of equal adc values.  Then, there may
c     be >1 maximum assigned to it.  Also, a true local max adjacent to a
c     plateau may give rise to "fake" local maxima on the plateau.
c

      nmax=0
      do jpix=1,nclspix
         indx=index_list(jpix)
         jpad=ipad(indx)
         jtdc=itdc(indx)
         if (height(jpad,jtdc).lt.i_minimum_height) goto 20 ! done
         if (allowed(jpad,jtdc)) then ! possible local max
c before storing, we check peak:valley for pixels along lines which
c  connect this local max to the bigger (previous) ones... note that
c  this check does not attempt to kill the biggest (first) max found.
c if peak:valley test fails, we do not store and just jump to line 19
            if (tclpar(1).mf_pv.gt.1.0) then ! (if not, then do not even 
                                             !  check - peak is ALWAYS > vall)
               do imax=1,nmax
                  dpad = max_pad(imax) - jpad
                  dtdc = max_tdc(imax) - jtdc
                  if (dpad.ne.0) then ! walk along line connecting peaks
                     slope = float(dtdc)/float(dpad)
                     valley = height(jpad,jtdc)
                     if (ABS(slope).lt.1.) then ! loop over pad
                                                        ! tdc depends
                                                        ! on pad
c RRB: Must have at least 3 pads for next loop...
                        do kpad=min(jpad,max_pad(imax))+1,
     +                          max(jpad,max_pad(imax))-1
                           ktdc=jtdc+nint(slope*float(kpad-jpad))
                           valley = min(valley,height(kpad,ktdc))
                        enddo
                     else                   ! loop over tdc
                                            ! pad depends on tdc
c RRB: Must have at least 3 buckets for next loop...
                        do ktdc=min(jtdc,max_tdc(imax))+1,
     +                          max(jtdc,max_tdc(imax))-1
                           kpad=jpad+nint(float(ktdc-jtdc)/slope)
                           valley = min(valley,height(kpad,ktdc))
                        enddo
                     endif
                  else          ! peaks on same pad.. walk in time only
                     valley = height(jpad,jtdc)
                     do ktdc=min(jtdc,max_tdc(imax))+1,
     +                       max(jtdc,max_tdc(imax))-1
                        valley = min(valley,height(jpad,ktdc))
                     enddo
                  endif

c RRB: ??? Need a more sophisticated algorithm, allowing for ADC & noise?
c--------------------------------------------
c we should NOT be using peak/valley!!!
c instead use peak-valley (i.e. peak MINUS valley)
c It was my stupidity.  malisa 4sep97
c                  if (valley.ne.0) then
c                     peak_to_valley = 
c     +                    float(height(jpad,jtdc))/float(valley)
c                     if (peak_to_valley.lt.tclpar(1).mf_pv)
c     +                    goto 19 ! FAILED
c                  endif
                  peak_to_valley = height(jpad,jtdc)-valley
                  if (peak_to_valley.lt.tclpar(1).mf_pv)
     +                 goto 19  ! FAILED
c--------------------------------------------
               enddo
            endif
c ok, passed peak:valley test... store this peak
            nmax=nmax+1
            if (nmax.gt.nmax_max) then
               nmax = nmax_max
c ??? Direct write is forbidden
               write(6,*)'Hey!... too many peaks... I am bailing...'
               do iwriteout=1,nclspix
c ??? Direct write is forbidden
                  write(56,*)' '
c ??? Direct write is forbidden
                  write(56,*)iwriteout,
     +                 ipad(iwriteout),itdc(iwriteout),iadc(iwriteout)
               enddo
               goto 20
            endif
            max_pad(nmax) = jpad
            max_tdc(nmax) = jtdc
            max_height(nmax)=iadc(indx)
 19         continue
c lay waste to adjacent area (may be large)...
c RRB: Reverse memory access order; mark false only within cluster region:
            do ktdc=max(jtdc-(iclosest_tdc-1),itdc_lo),
     +              min(jtdc+(iclosest_tdc-1),itdc_hi)
               do kpad=max(jpad-(iclosest_pad-1),ipad_lo),
     +                 min(jpad+(iclosest_pad-1),ipad_hi)
                  allowed(kpad,ktdc) = .false.
               enddo
            enddo
         else
c lay waste to some adjacent area even if not a peak......
c RRB: i.e., "allowed" was false for pixel
c RRB: mark false only within cluster region:
            do ktdc=max(jtdc-1,itdc_lo),
     +              min(jtdc+1,itdc_hi)
               do kpad=max(jpad-1,ipad_lo),
     +                 min(jpad+1,ipad_hi)
                  allowed(kpad,ktdc) = .false.
               enddo
            enddo
         endif
      enddo

 20   continue

c calculate "positions"...
      nhits = nmax
c
      if (nmax.le.0) then            ! nothing to fit
         goto 999
c
      else if (nmax.eq.1) then       ! use whole cluster in fitting
         call tph_fit_isolated_cluster(tclpar, nclspix,
     +        ipad,itdc,iadc,itrk,
     +        white_noise, filtered_noise,qq,
     +        x,dx,prf,
     +        z,dz,zrf,
     +        phi,track,
     +        mu1,mu2,mu3,mu4,mu5,
     +        npads, irow, isec,
     +        ipad_lo, ipad_hi, itdc_lo, itdc_hi)
         goto 999
      end if
c
c Initialize the pixel assignment array within the used TDC x pad rectangle:
c -1=below threshold; 0=unassigned
      do ktdc=itdc_lo,itdc_hi
         do kpad=ipad_lo,ipad_hi
            if (height(kpad,ktdc).le.0) then
               assign(kpad,ktdc) = -1
            else
               assign(kpad,ktdc) = 0
            end if
         end do
      end do
c
c Order maxima according to increasing peak amplitude:
      call sortzv(max_height,peak_list,nmax,-1,0,0)
c
c Range of automatic pixel assignment around local maxima:
      infl_pad = MAX (1,(iclosest_pad-1)/2)
      infl_tdc = MAX (1,(iclosest_tdc-1)/2)
c
c Auto-assign pixels close to local maxima
c (pixels end up assigned to smaller maxima, if there is a conflict):
      do i=1,nmax
         imax = peak_list(i)
         jpad = max_pad(imax)
         jtdc = max_tdc(imax)
c
         kpad_lo = max(jpad-infl_pad,ipad_lo)
         kpad_hi = min(jpad+infl_pad,ipad_hi)
         ktdc_lo = max(jtdc-infl_tdc,itdc_lo)
         ktdc_hi = min(jtdc+infl_tdc,itdc_hi)
c
         do ktdc=ktdc_lo, ktdc_hi
            do kpad=kpad_lo, kpad_hi
               if (assign(kpad,ktdc).eq.0) assign(kpad,ktdc)=imax
            enddo
         enddo
      enddo
c
c Expand region of assigned pixels down the slopes around the peaks:
      do i=1,nmax
         imax = peak_list(i)
c
         jpad = max_pad(imax)
         jtdc = max_tdc(imax)
c
c Record current rectilinear limits to hit:
         kpad_lo = max(jpad-infl_pad,ipad_lo)
         kpad_hi = min(jpad+infl_pad,ipad_hi)
         ipad_lo_temp = kpad_lo
         ipad_hi_temp = kpad_hi
c
         ktdc_lo = max(jtdc-infl_tdc,itdc_lo)
         ktdc_hi = min(jtdc+infl_tdc,itdc_hi)
         itdc_lo_temp = ktdc_lo
         itdc_hi_temp = ktdc_hi
c
c Record directions in which spreading is possible:
         kpad_lo_sprd = (kpad_lo-1.ge.ipad_lo)
         kpad_hi_sprd = (kpad_hi+1.le.ipad_hi)
         ktdc_lo_sprd = (ktdc_lo-1.ge.itdc_lo)
         ktdc_hi_sprd = (ktdc_hi+1.le.itdc_hi)
c
c Find endpoints of next pixel segments:
         ktdc_lo_lopad = MAX(ktdc_lo-1,itdc_lo)
         ktdc_hi_lopad = MIN(ktdc_hi+1,itdc_hi)
         ktdc_lo_hipad = MAX(ktdc_lo-1,itdc_lo)
         ktdc_hi_hipad = MIN(ktdc_hi+1,itdc_hi)
c
         kpad_lo_lotdc = kpad_lo
         kpad_hi_lotdc = kpad_hi
         kpad_lo_hitdc = kpad_lo
         kpad_hi_hitdc = kpad_hi
c
c Explore successively larger one-pixel-wide frames until we have reached
c the lowest slopes of this peak:
         do i_sprd=1, max(itdc_hi-ktdc_hi, ktdc_lo-itdc_lo,
     +                    ipad_hi-kpad_hi, kpad_lo-ipad_lo)
c
c Check one layer of pixel segments; first, check along lower pad edge:
            if (kpad_lo_sprd) then
               kpad_lo_sprd = .false.
               kpad_0  = kpad_lo
               kpad    = kpad_lo - 1
               kpad_lo = kpad
               dp = FLOAT(kpad-jpad)
               do ktdc=ktdc_lo_lopad, ktdc_hi_lopad
                  if (assign(kpad,ktdc).eq.0) then
                     dt = FLOAT(ktdc-jtdc)
                     ktdc_0 = jtdc + NINT(dt*(FLOAT(kpad_0-jpad)/dp))
                     if (assign(kpad_0,ktdc_0).eq.imax .and.
     &                   height(kpad,ktdc).lt.
     &                   height(kpad_0,ktdc_0)+h_tol) then
                        assign(kpad,ktdc) = imax
c
                        if (.not.kpad_lo_sprd) then
                           kpad_lo_sprd = .true.
                           ktdc_lo_lopad_t = ktdc
                        end if
                        ktdc_hi_lopad_t = ktdc
                     end if
                  end if
               end do
c
c Update tdc, pad limits for hit:
               if (kpad_lo_sprd) then
                  ipad_lo_temp = MIN(ipad_lo_temp,kpad)
                  itdc_lo_temp = MIN(itdc_lo_temp,ktdc_lo_lopad_t)
                  itdc_hi_temp = MAX(itdc_hi_temp,ktdc_hi_lopad_t)
c
                  kpad_lo_sprd = (kpad-1.ge.ipad_lo)
                  if (kpad_lo_sprd) then
                     ktdc_lo_lopad = MAX(ktdc_lo_lopad_t-1,itdc_lo)
                     ktdc_hi_lopad = MIN(ktdc_hi_lopad_t+1,itdc_hi)
                  end if
               end if
            end if
c
c Check along upper pad edge:
            if (kpad_hi_sprd) then
               kpad_hi_sprd = .false.
               kpad_0  = kpad_hi
               kpad    = kpad_hi + 1
               kpad_hi = kpad
               dp = FLOAT(kpad-jpad)
               do ktdc=ktdc_lo_hipad, ktdc_hi_hipad
                  if (assign(kpad,ktdc).eq.0) then
                     dt = FLOAT(ktdc-jtdc)
                     ktdc_0 = jtdc + NINT(dt*(FLOAT(kpad_0-jpad)/dp))
                     if (assign(kpad_0,ktdc_0).eq.imax .and.
     &                   height(kpad,ktdc).lt.
     &                   height(kpad_0,ktdc_0)+h_tol) then
                        assign(kpad,ktdc) = imax

                        if (.not.kpad_hi_sprd) then
                           kpad_hi_sprd = .true.
                           ktdc_lo_hipad_t = ktdc
                        end if
                        ktdc_hi_hipad_t = ktdc
                     end if
                  end if
               end do
c
c Update tdc, pad limits for hit:
               if (kpad_hi_sprd) then
                  ipad_hi_temp = MAX(ipad_hi_temp,kpad)
                  itdc_lo_temp = MIN(itdc_lo_temp,ktdc_lo_hipad_t)
                  itdc_hi_temp = MAX(itdc_hi_temp,ktdc_hi_hipad_t)
c
                  kpad_hi_sprd = (kpad+1.le.ipad_hi)
                  if (kpad_hi_sprd) then
                     ktdc_lo_hipad = MAX(ktdc_lo_hipad_t-1,itdc_lo)
                     ktdc_hi_hipad = MIN(ktdc_hi_hipad_t+1,itdc_hi)
                  end if
               end if
            end if
c
c Check along lower edge in tdc:
            if (ktdc_lo_sprd) then
               ktdc_lo_sprd = .false.
               ktdc_0  = ktdc_lo
               ktdc    = ktdc_lo - 1
               ktdc_lo = ktdc
               dt = FLOAT(ktdc-jtdc)
               do kpad=kpad_lo_lotdc, kpad_hi_lotdc
                  if (assign(kpad,ktdc).eq.0) then
                     dp = FLOAT(kpad-jpad)
                     kpad_0 = jpad + NINT(dp*(FLOAT(ktdc_0-jtdc)/dt))
                     if (assign(kpad_0,ktdc_0).eq.imax .and.
     &                    height(kpad,ktdc).lt.
     &                    height(kpad_0,ktdc_0)+h_tol) then
                        assign(kpad,ktdc) = imax
c
                        if (.not.ktdc_lo_sprd) then
                           ktdc_lo_sprd = .true.
                           kpad_lo_lotdc_t = kpad
                        end if
                        kpad_hi_lotdc_t = kpad
                     end if
                  end if
               end do
c
c Update tdc, pad limits for hit:
               if (ktdc_lo_sprd) then
                  itdc_lo_temp = MIN(itdc_lo_temp,ktdc)
                  ipad_lo_temp = MIN(ipad_lo_temp,kpad_lo_lotdc_t)
                  ipad_hi_temp = MAX(ipad_hi_temp,kpad_hi_lotdc_t)
c
                  ktdc_lo_sprd = (ktdc-1.ge.itdc_lo)
                  if (ktdc_lo_sprd) then
                     kpad_lo_lotdc = MAX(kpad_lo_lotdc_t-1,ipad_lo)
                     kpad_hi_lotdc = MIN(kpad_hi_lotdc_t+1,ipad_hi)
                  end if
               end if
            end if
c
c Check along upper edge in tdc:
            if (ktdc_hi_sprd) then
               ktdc_hi_sprd = .false.
               ktdc_0  = ktdc_hi
               ktdc    = ktdc_hi + 1
               ktdc_hi = ktdc
               dt = FLOAT(ktdc-jtdc)
               do kpad=kpad_lo_hitdc, kpad_hi_hitdc
                  if (assign(kpad,ktdc).eq.0) then
                     dp = FLOAT(kpad-jpad)
                     kpad_0 = jpad + NINT(dp*(FLOAT(ktdc_0-jtdc)/dt))
                     if (assign(kpad_0,ktdc_0).eq.imax .and.
     &                    height(kpad,ktdc).lt.
     &                    height(kpad_0,ktdc_0)+h_tol) then
                        assign(kpad,ktdc) = imax
c
                        if (.not.ktdc_hi_sprd) then
                           ktdc_hi_sprd = .true.
                           kpad_lo_hitdc_t = kpad
                        end if
                        kpad_hi_hitdc_t = kpad
                     end if
                  end if
               end do
c
c Update tdc, pad limits for hit:
               if (ktdc_hi_sprd) then
                  itdc_hi_temp = MAX(itdc_hi_temp,ktdc)
                  ipad_lo_temp = MIN(ipad_lo_temp,kpad_lo_hitdc_t)
                  ipad_hi_temp = MAX(ipad_hi_temp,kpad_hi_hitdc_t)
c
                  ktdc_hi_sprd = (ktdc+1.le.itdc_hi)
                  if (ktdc_hi_sprd) then
                     kpad_lo_hitdc = MAX(kpad_lo_hitdc_t-1,ipad_lo)
                     kpad_hi_hitdc = MIN(kpad_hi_hitdc_t+1,ipad_hi)
                  end if
               end if
            end if
c
c If no more pixels can be assigned in any direction,
c we're done finding the hit:
            if (.not.(kpad_lo_sprd .or.
     &                kpad_hi_sprd .or.
     &                ktdc_lo_sprd .or.
     &                ktdc_hi_sprd)) goto 90
c
         end do                 ! i_sprd=1,max()
c
 90      continue
c
c Load the assigned pixels:
         nclspix_temp = 0
         do ktdc=itdc_lo_temp, itdc_hi_temp
            do kpad=ipad_lo_temp, ipad_hi_temp
               if (assign(kpad,ktdc).eq.imax) then
c
c Guarantee that arrays won't overrun:
                  if (nclspix_temp.ge.nclpix_max) then
                     write (6,*) ' Too many pixels in hit ?'
                     goto 99
                  end if
c
                  nclspix_temp = nclspix_temp + 1
                  ipad_temp(nclspix_temp) = kpad
                  itdc_temp(nclspix_temp) = ktdc
                  iadc_temp(nclspix_temp) = height(kpad,ktdc)
                  itrk_temp(nclspix_temp) = track_pix(kpad,ktdc)
               endif
            enddo
         enddo
c
c Fit the peak:
 99      continue
         if (nclspix_temp.gt.0) then
*********
*********
*********	This is a VERY TEMPORARY PATCH
*********
*********
            ipad_hi_temp=-1
            ipad_lo_temp=1000
            itdc_hi_temp=-1
            itdc_lo_temp=1000
            do ii=1,nclspix_temp
               if(itdc_temp(ii).gt.itdc_hi_temp) itdc_hi_temp=itdc_temp(ii)
               if(itdc_temp(ii).lt.itdc_lo_temp) itdc_lo_temp=itdc_temp(ii)
               if(ipad_temp(ii).gt.ipad_hi_temp) ipad_hi_temp=ipad_temp(ii)
               if(ipad_temp(ii).lt.ipad_lo_temp) ipad_lo_temp=ipad_temp(ii)
            end do
*********
*********
*********      End of the very temporary fix
*********
*********
            call tph_fit_isolated_cluster(tclpar, nclspix_temp,
     +           ipad_temp,itdc_temp,iadc_temp,itrk_temp,
     +           white_noise, filtered_noise, qq(i),
     +           x(i),dx(i),prf(i),
     +           z(i),dz(i),zrf(i),
     +           phi(i),track(i),
     +           mu1(i),mu2(i),mu3(i),mu4(i),mu5(i),
     +           npads, irow, isec,
     +           ipad_lo_temp, ipad_hi_temp, itdc_lo_temp, itdc_hi_temp)
         else
c
c Should *not* be possible...
            nhits = nhits - 1
            write (6,*) ' No pixels in hit ?'
         end if
      end do   ! i=1,nmax
c
c Finish up:
c
 999  continue
c finally, put back array allowed the way you found it.
c RRB: Now simplified; also, memory access order reversed
      do jtdc=itdc_lo,itdc_hi
         do jpad=ipad_lo,ipad_hi
            allowed(jpad,jtdc) = .true.
         enddo
      enddo
      return
      end
