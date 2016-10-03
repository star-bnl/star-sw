* CB1.F
* the two-point tensor coefficient of p_mu for complex parameters
* this file is part of LoopTools
* last modified 25 Jun 04 th

#include "lt.h"


	double complex function CB1(p, m1, m2)
	implicit none
	double complex p, m1, m2

#include "ff.h"

	double complex a0i(2), b0p, b1p
	common /bsave/ a0i, b0p, b1p

	integer ier

	ldot = .TRUE.
	ier = 0
	call ffcb0(b0p, p, m1, m2, ier)
	call ffca0(a0i(1), m1, ier)
	call ffca0(a0i(2), m2, ier)
	call ffcb1(CB1, b0p, a0i, p, m1, m2, cfpij2, ier)
	ldot = .FALSE.

	if( ier .gt. MAXDIGITS ) print *,
     &    "CB1 lost ", ier, " digits (m1 = ", sqrt(dble(m1)), ")"
	end

************************************************************************

	subroutine cb1sub(result, p, m1, m2)
	implicit none
	double complex result
	double complex p, m1, m2

	double complex CB1
	external CB1

	result = CB1(p, m1, m2)
	end

