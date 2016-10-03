* A0.F
* the scalar one-point function
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "lt.h"


	double complex function A0(m)
	implicit none
	double precision m

#ifdef CHECK
	external A0check
#endif

	integer ier

	ier = 0
	call ffxa0(A0, m, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "A0 lost ", ier, " digits (m = ", sqrt(m), ")"

#ifdef CHECK
	call checkA(A0, A0check, "A0", m)
#endif
	end

************************************************************************

	subroutine a0sub(result, m)
	implicit none
	double complex result
	double precision m

	double complex A0
	external A0

	result = A0(m)
	end

