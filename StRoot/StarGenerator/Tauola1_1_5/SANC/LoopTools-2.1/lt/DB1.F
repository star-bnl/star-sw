* DB1.F
* the derivative of B1 with respect to p^2
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "lt.h"


	double complex function DB1(p, m1, m2)
	implicit none
	double precision p, m1, m2

#ifdef CHECK
	external DB1check
#endif

	integer ier

	ier = 0
	call ffxdb1(DB1, p, m1, m2, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "DB1 lost ", ier, " digits (m1 = ", sqrt(m1), ")"

#ifdef CHECK
	call checkB(DB1, DB1check, "DB1", p, m1, m2)
#endif
	end

************************************************************************

	subroutine db1sub(result, p, m1, m2)
	implicit none
	double complex result
	double precision p, m1, m2

	double complex DB1
	external DB1

	result = DB1(p, m1, m2)
	end

