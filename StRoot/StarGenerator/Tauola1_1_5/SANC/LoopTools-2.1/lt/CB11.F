* CB11.F
* the two-point tensor coefficient of p_mu p_nu for complex parameters
* this file is part of LoopTools
* last modified 28 Jun 04 th

#include "lt.h"


	double complex function CB11(p, m1, m2)
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
     &    "CB11 lost ", ier, " digits (m1 = ", sqrt(dble(m1)), ")"

	CB11 = b2(1)
	end

************************************************************************

	subroutine cb11sub(result, p, m1, m2)
	implicit none
	double complex result
	double complex p, m1, m2

	double complex CB11
	external CB11

	result = CB11(p, m1, m2)
	end

