* $Id: tph_3point_gauss.F,v 1.6 2000/02/03 00:05:13 snelling Exp $
* $Log: tph_3point_gauss.F,v $
* Revision 1.6  2000/02/03 00:05:13  snelling
* Changed warning reporting, improved switching between 3point Gaussian fit
* and weighted mean.
*
* Revision 1.5  2000/01/20 23:15:33  snelling
* Made 3point gaussian default
*
	integer function tph_3point_gauss(x, y, dy, ipad_lo, ipad_hi,
     &    npads, centroid, sigma, uncertainty)
c-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     x		- x coordinate for each point
C     y		- y(x) distribution to fit to gaussian
C     dy	- uncertainties on y(x)
C     ipad_lo	- first pad number in cluster
C     ipad_hi	- last pad number in cluster
C     npads	- last pad number in row
C
C   Output arguments : 
C
C     centroid 	- Gaussian centroid from 3-point fit to y(x)
C     sigma	- Gaussian width from 3-point fit to y(x)
C     uncertainty - uncertainty in centroid
C
C  Returns:
C
C     +1 if successful, -1 if unable to fit Gaussian
C     
C   Functional Description : 
C
C	Performs 3-point gaussian fit to passed distribution y(x)
C
C   Originally written 25 November 1994 by Mike Lisa
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modifications :
c   =============
C
C   Modified: 20-DEC-1996 by R.Bossingham, Lawrence Berkeley Natl. Laboratory
C             If the Gaussian width is more than 83% of the pad width,
C             return a function value of -2; do not calculate position.
C
C             03-NOV-1997 by R.Bossingham
C             Add calling arguments so as to clearly distinguish
C               a) range of pads involved in cluster
C               b) length of pad row
C             and change logic to use this information properly.
C             
C
C   Error conditions : 
C   ================
C
C     If the distribution is not well described by a gaussian, (i.e.
C	it has a peak on one edge and is not monotonic) the function
C       returns a value of -1
C
C     If the width is so large that a weighted mean algorithm is likely
C       to be more accurate, the function returns a value of -2
C
C-----------------------------------------------------------------------
c passed variables:
c
        integer ipad_lo, ipad_hi      ! Pad number extent for cluster
        integer npads                 ! Number of pads in cluster's pad row

	real centroid                 ! Centroid of hit along pad row
	real dy(*)
	real sigma                    ! Width of hit
	real uncertainty              ! Uncertainty in centroid
	real x0                       ! Hit centroid in offset coordinates
	real x(*)                     ! X positions
	real y(*)                     ! Amplitudes at x
c-----------------------------------------------------------------------
c local variables:
c
	integer imax                  ! Pad with largest signal
	integer npts                  ! Number of pads involved in hit

	real delta
	real r213
	real y1, y2, y3

c message call related variables
        character*132 m132

c-----------------------------------------------------------------------
c functions:
c
	integer lvmax
c-----------------------------------------------------------------------
c       debug information
	logical debug 
	data    debug /.false./

c       only use three pad hits or more
        npts = ipad_hi - ipad_lo + 1
        if (npts .lt. 3) then
           tph_3point_gauss = -1
           return
        else
           tph_3point_gauss = 1
        endif

c       Find pad with largest signal:
	imax = lvmax(y(ipad_lo),npts) + ipad_lo - 1

c       Check if maximum is not on an edge pad
        if ((imax .eq. 1) .or. (imax .eq. npads)) then	
	   if(debug) then
	      write(m132,*) 'TPH_3POINT_GAUSS maximum at sector edge'
	      call StWarning(m132)
	   endif
	   tph_3point_gauss = -1
	   return 
	endif

c       Check if maximum is not the first or last pad in the hit
	if ((imax .eq. ipad_lo) .or. (imax .eq. ipad_hi)) then
	   if(debug) then
	      write(m132,*) 'TPH_3POINT_GAUSS maximum at last or first pad'
	      call StWarning(m132)
	   endif
	   tph_3point_gauss = -1
	   return
	endif

c       fill the three points to fit
	y1 = y(imax - 1)
	y2 = y(imax)
	y3 = y(imax + 1)

c       Check if ADC values are not zero
        if (y1*y3 .lt. 1.0e-10) then 
	   if(debug) then
	      write(m132,*) 'TPH_3POINT_GAUSS partly zero ADC hit'
	      call StWarning(m132)
	   endif
           tph_3point_gauss = -1
           return
        endif

c       Check cluster shape
	r213 = log((y2*y2)/(y1*y3))

        if (r213 .le. 0.0) then
	   if(debug) then
	      write(m132,*) 'TPH_3POINT_GAUSS bad shape - r213'
	      call StWarning(m132)
	   endif
           tph_3point_gauss = -1
           return
	endif

c       From Monte Carlo, if the fitted Gaussian is wider than about 83%
c       of the pad pitch in X, the weighted mean algorithm is more accurate;
c       we return a diagnostic to force its use.  [ 1/sqrt(1.452)=0.83 ].
	if (r213 .le. 1.452) then
	   if(debug) then
	      write(m132,*) 'TPH_3POINT_GAUSS to broad Gaussian'
	      call StWarning(m132)
	   endif
           tph_3point_gauss = -2
           return
        endif

c       RRB: Slightly recode to avoid direct comparison of f
c       loating point numbers
	delta = x(imax) - x(imax - 1)

	x0 = (delta/2.0) * log(y3/y1) / r213
	centroid = x(imax) + x0
	sigma = delta / sqrt(r213)

	uncertainty = sqrt( (dy(imax - 1) * (delta/2.0-x0)/y(imax - 1))**2 +
     &			    (dy(imax)*(2.0*x0)/y(imax))**2 +
     &			    (dy(imax + 1)*(delta/2.0+x0)/y(imax + 1))**2
     &				) / r213

	return
	end
