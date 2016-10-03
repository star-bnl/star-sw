* DB1check.F
* the derivative of B1 with respect to p^2
* this function is adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* it is used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "DB1.F"


	double complex function DB1check(p, m1, m2)
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
	  if( m2 .eq. 0 .and. abs(p - m1) .lt. CALACC ) then
	    DB1check = .5D0*(3 + log(lambda2/p))/p
	    return
	  endif
* general case
	  call roots(p, m1, m2, x1, x2, y1, y2, r)
	  if( abs(x1 - x2) .gt. CALACC*abs(x1 + x2) ) then
	    DB1check = (yfpv(2, x1, y1) - yfpv(2, x2, y2))/r
	  else if( abs(x1) .gt. 10 ) then
	    DB1check =
     &        dble((2/3D0 + (2 - 3*x1)*fpv(3, x1, y1))/x1**2)/p
	  else if( abs(y1) .gt. CALACC ) then
	    DB1check =
     &        dble(3/2D0 + (2 - 3*x1)*fpv(1, x1, y1))/p
	  else
	    print *, "DB1(", p, ",", m1, ",", m2, ") not defined"
	    DB1check = 999D300
	  endif
* zero momentum
	else if( abs(m1 - m2) .gt. minacc ) then
	  x2 = ONEmEPS*m1/(m1 - m2)
	  y2 = ONEmEPS*m2/(m2 - m1)
	  if( abs(x2) .lt. 10 ) then
	    DB1check = -(1/3D0 + yfpv(2, x2, y2))/(m1 - m2)
	  else
	    DB1check = -(1/3D0 + yfpv(3, x2, y2))/m1
	  endif
	else
	  DB1check = -1/12D0/m1
	endif
	end

