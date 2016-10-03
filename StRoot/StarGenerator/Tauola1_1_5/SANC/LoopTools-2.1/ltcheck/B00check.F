* B00check.F
* the two-point tensor coefficient of g_{mu nu}
* this function is adapted from Ansgar Denner's bcanew.f
* to the conventions of LoopTools;
* it is used for double-checking the results of FF
* last modified 16 Jun 04 th

#include "ltcheck.h"
#include "B00.F"


	double complex function B00check(p, m1, m2)
	implicit none
	double precision p, m1, m2

	double complex A0check, B0check, B1check
	external A0check, B0check, B1check

	if( abs(p) .gt. CALACC*(abs(p) + m1 + m2) ) then
	  B00check = ((p + m1 - m2)*B1check(p, m1, m2) +
     &      2*m1*B0check(p, m1, m2) + A0check(m2) +
     &      m1 + m2 - p/3D0)/6D0
	else
	  B00check = (2*(m2*B0check(0D0, m1, m2) +
     &      A0check(m1)) + m1 + m2)/8D0
	endif
	end

