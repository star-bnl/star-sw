* $Id: tph_weighted_mean.F,v 1.1 1998/01/27 00:49:18 fisyak Exp $
* $Log: tph_weighted_mean.F,v $
* Revision 1.1  1998/01/27 00:49:18  fisyak
* Split sources
*
	Subroutine tph_weighted_mean(x, y, dy_cor,dy_uncor,
     +     ilo, ihi, imax,
     +     mom1, mom2, uncertainty)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     x		- x coordinate of each point
C     y		- y(x) distribution
C     dy_cor	- uncertainty in y(x) that is CORRELATED
C     dy_uncor	- uncertainty in y(x) that is UNCORRELATED
C     ilo	- index of first sample in distribution
C     ihi	- index of last sample in distribution
C     imax	- max. possible index for sample
C
C   Output arguments : 
C
C     mom1	- first moment of y(x) distribution (mean x)
C     mom2	- second moment of y(x) distribution (rms)
C     uncertainty - uncertainty of mean
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C
C	takes first and second moment of input distribution
C
C   Originally written 25 November 1994 by Mike Lisa
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modifications:
c   ==============
C
C     27dec95 - mal - calculate uncertainty in mean using
C                     correlated and uncorrelated uncertainty in y
C
C   Error conditions : 
C   ================
C
C-----------------------------------------------------------------------
c passed variables:
c
        integer ilo                   ! Lower index for input points
        integer ihi                   ! Upper index for input points
        integer imax                  ! Highest allowed index

	real dy_cor(*)                ! Correlated amplitude rms
	real dy_uncor(*)              ! Uncorrelated amplitude rms
	real mom1                     ! First moment of distribution
	real mom2                     ! Second moment of distribution
	real uncertainty              ! Quad. sum of dy_cor, dy_uncor effects
	real uncertainty_cor          ! Uncertainty due to dy_cor
	real uncertainty_uncor        ! Uncertainty due to dy_uncor
	real x(*)                     ! Coordinates of points
	real y(*)                     ! Amplitudes at points
C-----------------------------------------------------------------------
c local variables:
c
	integer ipt                   ! Index for points
	integer npts                  ! Number of points to use

	real ysum                     ! Sum of y values
C-----------------------------------------------------------------------

        npts = ihi - ilo + 1

	mom1 = 0.0
	mom2 = 0.0
	ysum = 0.0
	do ipt=ilo,ihi
	  ysum = ysum + y(ipt)
	  mom1 = mom1 + x(ipt)*y(ipt)
          mom2 = mom2 + x(ipt)*(x(ipt)*y(ipt))
	enddo
	mom1 = mom1 / ysum
        mom2 = sqrt(mom2/ysum - mom1**2)
c
c the uncertainty from the correlated noise and uncorrelated noise
c  is estimated below...
	uncertainty_cor = 0.0
	uncertainty_uncor = 0.0
	do ipt=ilo,ihi
	  uncertainty_cor = uncertainty_cor+
     +          abs(x(ipt)-mom1)*dy_cor(ipt)
	  uncertainty_uncor = uncertainty_uncor+
     +          ((x(ipt)-mom1)*dy_uncor(ipt))**2
	enddo
        uncertainty_cor = uncertainty_cor/ysum
	uncertainty_uncor = sqrt(uncertainty_uncor)/ysum

	uncertainty = sqrt(uncertainty_cor**2+uncertainty_uncor**2)

	return
	end
