* DB0check.F
* the derivative of B0 with respect to p^2
* this function is adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* it is used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "DB0.F"


	double complex function DB0check(p, m1, m2)
	implicit none
	double precision p, m1, m2

#include "ff.h"

	double complex fpv, yfpv
	external fpv, yfpv

	double complex x1, x2, y1, y2, r
	double precision minacc

	minacc = CALACC*(m1 + m2)
	if( abs(p) .gt. minacc ) then
* IR divergent case
	  if( m1*m2 .eq. 0 .and. abs(p - m1 - m2) .lt. CALACC ) then
	    DB0check = -(1 + .5D0*log(lambda2/p))/p
	    return
	  endif
* general case
	  call roots(p, m1, m2, x1, x2, y1, y2, r)
	  if( abs(x1 - x2) .gt. CALACC*abs(x1 + x2) ) then
	    DB0check = (yfpv(1, x2, y2) - yfpv(1, x1, y1))/r
	  else if( abs(x1) .gt. 10 ) then
	    DB0check =
     &        dble(-(.5D0 + (1 - 2*x1)*fpv(2, x1, y1))/x1**2)/p
	  else if( abs(y1) .gt. CALACC .and. abs(x1) .gt. CALACC ) then
	    DB0check = -dble(2 + (1 - 2*x1)*fpv(0, x1, y1))/p
	  else
	    print *, "DB0(", p, ",", m1, ",", m2, ") not defined"
	    DB0check = 999D300
	  endif
* zero momentum
	else if( abs(m1 - m2) .gt. minacc ) then
	  x2 = ONEmEPS*m1/(m1 - m2)
	  y2 = ONEmEPS*m2/(m2 - m1)
	  if( abs(x2) .lt. 10 ) then
	    DB0check = (.5D0 + yfpv(1, x2, y2))/(m1 - m2)
	  else
	    DB0check = (.5D0 + yfpv(2, x2, y2))/m1
	  endif
	else
	  DB0check = 1/6D0/m1
	endif
	end

