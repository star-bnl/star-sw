* B1check.F
* the two-point tensor coefficient of p_mu
* this function is adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* it is used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "B1.F"


	double complex function B1check(p, m1, m2)
	implicit none
	double precision p, m1, m2

#include "ff.h"

	double complex fpv, xlogx
	external fpv, xlogx

	double complex x1, x2, y1, y2, r
	double precision minacc

	minacc = CALACC*(m1 + m2)
* general case
	if( abs(p) .gt. minacc ) then
	  call roots(p, m1, m2, x1, x2, y1, y2, r)
	  if( abs(y1) .gt. .5D0 .and. abs(y2) .gt. .5D0 ) then
	    B1check = .5D0*(log(m2/mudim2) +
     &        fpv(2, x1, y1) + fpv(2, x2, y2))
	  else if( abs(x1) .lt. 10 .and. abs(x2) .lt. 10 ) then
	    B1check = -.5D0*(1 - log(p*ONEmEPS/mudim2) +
     &        x1*xlogx(-x1) + x1 + x2*xlogx(-x2) + x2 -
     &        (1 + x1)*xlogx(y1) - (1 + x2)*xlogx(y2))
	  else if( abs(x1) .gt. .5D0 .and. abs(x2) .gt. .5D0 ) then
	    B1check = .5D0*(log(m1/mudim2) + 1 +
     &        (1 + x1)*fpv(1, y1, x1) + (1 + x2)*fpv(1, y2, x2))
	  else
	    print *, "B1(", p, ",", m1, ",", m2, ") not defined"
	    B1check = 999D300
	  endif
* zero momentum
	else if( abs(m1 - m2) .gt. minacc ) then
	  x2 = ONEmEPS*m1/(m1 - m2)
	  y2 = ONEmEPS*m2/(m2 - m1)
	  if( abs(y2) .gt. .5D0 ) then
	    B1check = .5D0*(log(m2/mudim2) + fpv(2, x2, y2))
	  else
	    B1check = .5D0*(log(m1/mudim2) +
     &        (1 + x2)*fpv(1, y2, x2) + .5D0)
	  endif
	else
	  B1check = .5D0*log(m2/mudim2)
	endif
	B1check = B1check - .5D0*divergence
	end

