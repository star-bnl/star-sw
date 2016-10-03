* B00.F
* the two-point tensor coefficient of g_{mu nu}
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "lt.h"


	double complex function B00(p, m1, m2)
	implicit none
	double precision p, m1, m2

#include "ff.h"

	double complex a0i(2), b0p, b1p
	common /bsave/ a0i, b0p, b1p

	double complex B1
	external B1

#ifdef CHECK
	external B00check
#endif

	double complex b2(2)
	integer ier

	b1p = B1(p, m1, m2)
	ier = 0
	call ffxb2p(b2, b1p, b0p, a0i, p, m1, m2, fpij2, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "B00 lost ", ier, " digits (m1 = ", sqrt(m1), ")"

	B00 = b2(2)

#ifdef CHECK
	call checkB(B00, B00check, "B00", p, m1, m2)
#endif
	end

************************************************************************

	subroutine b00sub(result, p, m1, m2)
	implicit none
	double complex result
	double precision p, m1, m2

	double complex B00
	external B00

	result = B00(p, m1, m2)
	end

