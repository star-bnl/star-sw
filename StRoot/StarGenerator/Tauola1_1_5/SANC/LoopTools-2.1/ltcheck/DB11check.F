* DB11check.F
* the derivative of B11 with respect to p^2
* this function is adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* it is used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "DB11.F"


	double complex function DB11check(p, m1, m2)
	implicit none
	double precision p, m1, m2

	double complex fpv, yfpv
	external fpv, yfpv

	double complex x1, x2, y1, y2, r
	double precision minacc

	minacc = CALACC*(m1 + m2)
* general case
	if( abs(p) .gt. minacc ) then
	  call roots(p, m1, m2, x1, x2, y1, y2, r)
	  if( abs(x1 - x2) .gt. CALACC*abs(x1 + x2) ) then
	    DB11check = (yfpv(3, x2, y2) - yfpv(3, x1, y1))/r
	    return
	  else if( abs(x1) .gt. 10 ) then
	    DB11check =
     &        dble((-3/4D0 + (4*x1 - 3)*fpv(4, x1, y1))/x1**2)/p
	    return
	  else if( abs(y1) .gt. CALACC ) then
	    DB11check =
     &        dble(-4/3D0 + (4*x1 - 3)*fpv(2, x1, y1))/p
	    return
	  endif
	endif
* zero momentum
	print *, "DB11(", p, ",", m1, ",", m2, ") not defined"
	DB11check = 999D300
	end

