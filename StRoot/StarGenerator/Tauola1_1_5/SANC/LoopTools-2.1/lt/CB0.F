* CB0.F
* the scalar two-point function for complex parameters
* this file is part of LoopTools
* last modified 25 Jun 04 th

#include "lt.h"


	double complex function CB0(p, m1, m2)
	implicit none
	double complex p, m1, m2

	integer ier

	ier = 0
	call ffcb0(CB0, p, m1, m2, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "CB0 lost ", ier, " digits (m1 = ", sqrt(dble(m1)), ")"
	end

************************************************************************

	subroutine cb0sub(result, p, m1, m2)
	implicit none
	double complex result
	double complex p, m1, m2

	double complex CB0
	external CB0

	result = CB0(p, m1, m2)
	end

