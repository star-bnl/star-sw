* CA0.F
* the scalar one-point function for complex mass
* this file is part of LoopTools
* last modified 25 Jun 04 th

#include "lt.h"


	double complex function CA0(m)
	implicit none
	double complex m

	integer ier

	ier = 0
	call ffca0(CA0, m, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "CA0 lost ", ier, " digits (m = ", sqrt(dble(m)), ")"
	end

************************************************************************

	subroutine ca0sub(result, m)
	implicit none
	double complex result
	double complex m

	double complex CA0
	external CA0

	result = CA0(m)
	end

