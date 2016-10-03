* B0check.F
* the scalar two-point function
* this function is adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* it is used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "B0.F"


	double complex function B0check(p, m1, m2)
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
	    B0check = -log(m2/mudim2) - 
     &        fpv(1, x1, y1) - fpv(1, x2, y2)
	  else if( abs(x1) .lt. 10 .and. abs(x2) .lt. 10 ) then
	    B0check = 2 - log(p*ONEmEPS/mudim2) +
     &        xlogx(-x1) + xlogx(-x2) - xlogx(y1) - xlogx(y2)
	  else if( abs(x1) .gt. .5D0 .and. abs(x2) .gt. .5D0 ) then
	    B0check = -log(m1/mudim2) -
     &        fpv(1, y1, x1) - fpv(1, y2, x2)
	  else
	    print *, "B0(", p, ",", m1, ",", m2, ") not defined"
	    B0check = 999D300
	  endif
* zero momentum
	else if( abs(m1 - m2) .gt. minacc ) then
	  x2 = ONEmEPS*m1/(m1 - m2)
	  y2 = ONEmEPS*m2/(m2 - m1)
	  if( abs(y2) .gt. .5D0 ) then
	    B0check = -log(m2/mudim2) - fpv(1, x2, y2)
	  else
	    B0check = -log(m1/mudim2) - fpv(1, y2, x2)
	  endif
	else
	  B0check = -log(m2/mudim2)
	endif
        B0check = B0check + divergence
	end

