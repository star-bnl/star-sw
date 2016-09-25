* CB00.F
* the two-point tensor coefficient of g_{mu nu} for complex parameters
* this file is part of LoopTools
* last modified 29 Jun 04 th

#include "lt.h"


	double complex function CB00(p, m1, m2)
	implicit none
	double complex p, m1, m2

#include "ff.h"

	double complex a0i(2), b0p, b1p
	common /bsave/ a0i, b0p, b1p

	double complex CB1
	external CB1

	double complex b2(2)
	integer ier

	b1p = CB1(p, m1, m2)
	ier = 0
	call ffcb2p(b2, b1p, b0p, a0i, p, m1, m2, cfpij2, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "CB00 lost ", ier, " digits (m1 = ", sqrt(dble(m1)), ")"

	CB00 = b2(2)
	end

************************************************************************

	subroutine cb00sub(result, p, m1, m2)
	implicit none
	double complex result
	double complex p, m1, m2

	double complex CB00
	external CB00

	result = CB00(p, m1, m2)
	end

