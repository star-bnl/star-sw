* CD0.F
* the scalar four-point function for complex parameters
* this file is part of LoopTools
* last modified 25 Jun 04 th

#include "lt.h"


	double complex function CD0(p1, p2, p3, p4,
     &    p1p2, p2p3, m1, m2, m3, m4)
	implicit none
	double complex p1, p2, p3, p4, p1p2, p2p3
	double complex m1, m2, m3, m4

	double complex xpi(13)
	integer ier

	xpi(1) = m1
	xpi(2) = m2
	xpi(3) = m3
	xpi(4) = m4
	xpi(5) = p1 
	xpi(6) = p2
	xpi(7) = p3
	xpi(8) = p4
	xpi(9) = p1p2
	xpi(10) = p2p3
	xpi(11) = 0
	xpi(12) = 0
	xpi(13) = 0

	ier = 0
	call ffcd0(CD0, xpi, ier)

	if( ier .gt. MAXDIGITS ) print *,
     &    "CD0 lost ", ier, " digits (m1 = ", sqrt(dble(m1)), ")"
	end

************************************************************************

	subroutine cd0sub(result, p1, p2, p3, p4,
     &    p1p2, p2p3, m1, m2, m3, m4)
	implicit none
	double complex result
	double complex p1, p2, p3, p4, p1p2, p2p3
	double complex m1, m2, m3, m4

	double complex CD0
	external CD0

	result = CD0(p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4)
	end

