* DB11.F
* the derivative of B11 with respect to p^2
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "lt.h"


	double complex function DB11(p, m1, m2)
	implicit none
	double precision p, m1, m2

#ifdef CHECK
	external DB11check
#endif

	integer ier

	ier = 0
	call ffxdb11(DB11, p, m1, m2, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "DB11 lost ", ier, " digits (m1 = ", sqrt(m1), ")"

#ifdef CHECK
	call checkB(DB11, DB11check, "DB11", p, m1, m2)
#endif
	end

************************************************************************

	subroutine db11sub(result, p, m1, m2)
	implicit none
	double complex result
	double precision p, m1, m2

	double complex DB11
	external DB11

	result = DB11(p, m1, m2)
	end

