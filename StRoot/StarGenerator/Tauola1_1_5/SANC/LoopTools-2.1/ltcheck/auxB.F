* auxB.F
* auxillary functions used by the two-point integrals
* these functions are adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* they are used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"


	subroutine roots(p, m1, m2, x1, x2, y1, y2, r)
	implicit none
	double precision p, m1, m2
	double complex x1, x2, y1, y2, r

	double precision q

	r = sqrt(dcmplx(p*(p - 2*(m1 + m2)) + (m1 - m2)**2))
	q = p + m1 - m2
	x1 = .5D0*(q + r)/p
	x2 = .5D0*(q - r)/p
	if( abs(x2) .gt. abs(x1) ) then
	  x1 = m1/p/x2
	else if( abs(x1) .gt. abs(x2) ) then
	  x2 = m1/p/x1
	endif
	x1 = x1 + dcmplx(0D0,  abs(p*x1)/p*EPS)
	x2 = x2 + dcmplx(0D0, -abs(p*x2)/p*EPS)

	q = p - m1 + m2
	y2 = (q + r)/2D0/p
	y1 = (q - r)/2D0/p
	if( abs(y2) .gt. abs(y1) ) then
	  y1 = m2/p/y2
	else if( abs(y1) .gt. abs(y2) ) then
	  y2 = m2/p/y1
	endif
	y1 = y1 + dcmplx(0D0, -abs(p*y1)/p*EPS)
	y2 = y2 + dcmplx(0D0,  abs(p*y2)/p*EPS)
	end

************************************************************************

	double complex function fpv(n, x, y)
	implicit none
	integer n
	double complex x, y

	integer m
	double complex xm

	if( abs(x) .lt. 10 ) then
	  if( n .eq. 0 ) then
	    fpv = -log(-y/x)
	  else if( abs(x) .lt. CALACC ) then
	    fpv = -1D0/n
	  else
	    fpv = 0
	    xm = 1
	    do m = 0, n - 1
	      fpv = fpv - xm/(n - m)
	      xm = xm*x
	    enddo
	    fpv = fpv - xm*log(-y/x)
	  endif
	else
	  fpv = 0
	  xm = 1
	  do m = 1, 30
	    xm = xm/x
	    fpv = fpv + xm/(m + n)
	    if( abs(xm/fpv) .lt. CALACC**2 ) return
	  enddo
	endif
	end

************************************************************************

	double complex function yfpv(n, x, y)
	implicit none
	integer n
	double complex x, y

	double complex fpv
	external fpv

	if( abs(y) .eq. 0 ) then
	  yfpv = 0
	else
	  yfpv = y*fpv(n, x, y)
	endif
	end

************************************************************************

	double complex function xlogx(x)
	implicit none
	double complex x

	if( abs(x) .eq. 0 ) then
	  xlogx = 0
	else
	  xlogx = x*log(x)
	endif
	end

