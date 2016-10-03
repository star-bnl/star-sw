* B11check.F
* the two-point tensor coefficient of p_mu p_nu
* this function is adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* it is used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "B11.F"


	double complex function B11check(p, m1, m2)
	implicit none
	double precision p, m1, m2

#include "ff.h"

	double complex fpv, yfpv, xlogx
	external fpv, yfpv, xlogx

	double complex x1, x2, y1, y2, r
	double precision minacc

	minacc = CALACC*(m1 + m2)
* general case
	if( abs(p) .gt. minacc ) then
	  call roots(p, m1, m2, x1, x2, y1, y2, r)
	  if( abs(y1) .gt. .5D0 .and. abs(y2) .gt. .5D0 ) then
	    B11check = (-log(m2/mudim2) -
     &        fpv(3, x1, y1) - fpv(3, x2, y2))/3D0
	  else if( abs(x1) .lt. 10 .and. abs(x2) .lt. 10 ) then
	    x1 = x1**2*(xlogx(-x1) + 1) + .5D0*x1 -
     &        (1 + x1*(1 + x1))*xlogx(y1)
	    x2 = x2**2*(xlogx(-x2) + 1) + .5D0*x2 -
     &        (1 + x2*(1 + x2))*xlogx(y2)
	    B11check = (2/3D0 - log(p*ONEmEPS/mudim2) +
     &        x1 + x2)/3D0
	  else if( abs(x1) .gt. .5D0 .and. abs(x2) .gt. .5D0 ) then
	    x1 = (1 + x1*(1 + x1))*fpv(1, y1, x1) + .5D0*x1
	    x2 = (1 + x2*(1 + x2))*fpv(1, y2, x2) + .5D0*x2
	    B11check = (-4/3D0 - log(m1/mudim2) - x1 - x2)/3D0
	  else
	    print *, "B11(", p, ",", m1, ",", m2, ") not defined"
	    B11check = 999D300
	  endif
* zero momentum
	else if( abs(m1 - m2) .gt. minacc ) then
	  x2 = ONEmEPS*m1/(m1 - m2)
	  y2 = ONEmEPS*m2/(m2 - m1)
	  if( abs(y2) .gt. .5D0 ) then
	    B11check = (-log(m2/mudim2) - fpv(3, x2, y2))/3D0
	  else
	    B11check = (-log(m1/mudim2) +
     &        (1 + x2*(1 + x2))*yfpv(0, x2, y2) +
     &        x2*(x2 + .5D0) + 1/3D0)/3D0
	  endif
	else
	  B11check = -log(m2/mudim2)/3D0
	endif
	B11check = B11check + divergence/3D0
	end

