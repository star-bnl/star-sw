* A0check.F
* the scalar one-point function
* this function is adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* it is used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "A0.F"


	double complex function A0check(m)
	implicit none
	double precision m

#include "ff.h"

	if( m .eq. 0 ) then
	  A0check = 0
	else
	  A0check = m*(1 - log(m/mudim2) + divergence)
	endif
	end

