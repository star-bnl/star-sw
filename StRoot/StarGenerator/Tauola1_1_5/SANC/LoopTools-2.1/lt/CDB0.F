* CDB0.F
* the derivative of B0 with respect to p^2 for complex parameters
* this file is part of LoopTools
* last modified 25 Jun 04 th

#include "lt.h"


	double complex function CDB0(p, m1, m2)
	implicit none
	double complex p, m1, m2

	double complex pdb0p
	integer ier

	ier = 0
	call ffcdb0(CDB0, pdb0p, p, m1, m2, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "CDB0 lost ", ier, " digits (m1 = ", sqrt(dble(m1)), ")"
	end

************************************************************************

	subroutine cdb0sub(result, p, m1, m2)
	implicit none
	double complex result
	double complex p, m1, m2

	double complex CDB0
	external CDB0

	result = CDB0(p, m1, m2)
	end

