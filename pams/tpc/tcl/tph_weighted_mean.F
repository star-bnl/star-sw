* $Id: tph_weighted_mean.F,v 1.4 2000/11/14 22:29:04 genevb Exp $
* $Log: tph_weighted_mean.F,v $
* Revision 1.4  2000/11/14 22:29:04  genevb
* Switched from float to double in many places
*
* Revision 1.3  1999/10/05 00:47:15  snelling
* the tdc range saga continues
*
* Revision 1.2  1998/07/22 23:15:33  hardtke
* fix NaN bug in tphit table
*
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
C     22Jul98 - DH  - check to make sure mom2 is positive before taking 
C                     square root.
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

	real*8 ysum                   ! Sum of y values
        real*8 tmp,unc_cor,unc_uncor,m1,m2
C-----------------------------------------------------------------------

        npts = ihi - ilo + 1

	m1 = 0.0
	m2 = 0.0
	ysum = 0.0
	do ipt=ilo,ihi
	  ysum = ysum + dble(y(ipt))
          tmp = dble(x(ipt))*dble(y(ipt))
	  m1 = m1 + tmp
          m2 = m2 + tmp*dble(x(ipt))
	enddo
	m1 = m1 / ysum
        m2 = m2/ysum - m1**2
        if (mom2.ge.0.) then
         m2=dsqrt(m2)
        else
         m2 = 0.0
        endif
c
c the uncertainty from the correlated noise and uncorrelated noise
c  is estimated below...
	unc_cor = 0.0
	unc_uncor = 0.0
	do ipt=ilo,ihi
          tmp = dble(x(ipt))-m1
	  unc_cor = unc_cor+dabs(tmp)*dy_cor(ipt)
	  unc_uncor = unc_uncor+(tmp*dy_uncor(ipt))**2
	enddo
        unc_cor = unc_cor/ysum
	unc_uncor = dsqrt(unc_uncor)/ysum

        uncertainty_cor = unc_cor
        uncertainty_uncor = unc_uncor
	uncertainty = dsqrt(unc_cor**2+unc_uncor**2)
        mom1 = m1
        mom2 = m2

	return
	end

