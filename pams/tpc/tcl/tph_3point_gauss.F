* $Id: tph_3point_gauss.F,v 1.1 1998/01/27 00:49:15 fisyak Exp $
* $Log: tph_3point_gauss.F,v $
* Revision 1.1  1998/01/27 00:49:15  fisyak
* Split sources
*
	integer function tph_3point_gauss(x, y, dy, ipad_lo, ipad_hi,
     +				npads, centroid, sigma, uncertainty)
C-----------------------------------------------------------------------

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
C-----------------------------------------------------------------------
c local variables:
c
        integer id                    ! Message ID
	integer imax                  ! Pad with largest signal
	integer istart                ! First pad
	integer npts                  ! Number of pads involved in hit

	real delta
	real r213
	real y1, y2, y3
C-----------------------------------------------------------------------
c functions:
c
	integer lvmax
C-----------------------------------------------------------------------

        npts = ipad_hi - ipad_lo + 1
        if (npts.lt.3) then
           tph_3point_gauss = -1
           return
        else
           tph_3point_gauss = 1
        endif
c
c Find pad with largest signal:
	imax = lvmax(y(ipad_lo),npts) + ipad_lo - 1
        if (imax.eq.1) then             ! maximum at first point
           if (y(2).le.y(3)) then
              id = 0
              tph_3point_gauss = -1
*              call message('TPH_3POINT_GAUSS bad shape', 1,id)
              write(6,*) 'TPH_3POINT_GAUSS bad shape'
              return
           endif
           istart = 1
	else if (imax.eq.npads) then ! maximum at last point
           if (y(npads-1).le.y(npads-2)) then
              id = 0
              tph_3point_gauss = -1
*              call message('TPH_3POINT_GAUSS bad shape', 1,id)
              write(6,*) 'TPH_3POINT_GAUSS bad shape'
              return
           endif
           istart = npads - 2
        else                            ! normal - max not at edge
           istart = imax - 1
	endif

	y1 = y(istart)
	y2 = y(istart+1)
	y3 = y(istart+2)

        if (y1*y3.lt.1.0e-10) then ! zero occupancy - no can do
           tph_3point_gauss = -1
           return
        endif

C RRB: Slightly recode to avoid direct comparison of floating pt. numbers
	delta = x(istart+1) - x(istart)
	if (ABS(delta-1.).gt.1.E-7) then
	  id = 0
*	  call message('TPH_3POINT_GAUSS unexpected delta .ne. 1', 1,id)
	  write(6,*) 'TPH_3POINT_GAUSS unexpected delta .ne. 1'
          tph_3point_gauss = -1
	  return
	endif

	r213 = log(y2*y2/(y1*y3))
        if (r213.le.0.0) then
           id = 0
           tph_3point_gauss = -1
*           call message('TPH_3POINT_GAUSS bad shape - r213', 1,id)
            write(6,*) 'TPH_3POINT_GAUSS bad shape - r213'
           return

********************** let's try to take it out....**********************
C
C From Monte Carlo, if the fitted Gaussian is wider than about 83%
C of the pad pitch in X, the weighted mean algorithm is more accurate;
C we return a diagnostic to force its use.  [ 1/sqrt(1.452)=0.83 ].
C	elseif (r213.le.1.452) then
C           id = 0
C           tph_3point_gauss = -2
C           return
        endif

	x0 = (delta/2.0) * log(y3/y1)/r213
	centroid = x(istart+1) + x0
	sigma = delta/sqrt(r213)

	uncertainty = sqrt( (dy(istart)*(delta/2.0-x0)/y(istart))**2 +
     +			    (dy(istart+1)*(2.0*x0)/y(istart+1))**2 +
     +			    (dy(istart+2)*(delta/2.0+x0)/y(istart+2))**2
     +				)/r213

	return
	end
