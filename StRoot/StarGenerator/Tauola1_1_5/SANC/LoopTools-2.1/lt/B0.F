* B0.F
* the scalar two-point function
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "lt.h"


	double complex function B0(p, m1, m2)
	implicit none
	double precision p, m1, m2

#ifdef CHECK
	external B0check
#endif

	integer ier

	ier = 0
	call ffxb0(B0, p, m1, m2, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "B0 lost ", ier, " digits (m1 = ", sqrt(m1), ")"

#ifdef CHECK
	call checkB(B0, B0check, "B0", p, m1, m2)
#endif
	end

************************************************************************

	subroutine b0sub(result, p, m1, m2)
	implicit none
	double complex result
	double precision p, m1, m2

	double complex B0
	external B0

	result = B0(p, m1, m2)
	end

