* CC0.F
* the scalar three-point function for complex parameters
* this file is part of LoopTools
* last modified 25 Jun 04 th

#include "lt.h"


	double complex function CC0(p1, p2, p1p2, m1, m2, m3)
	implicit none
	double complex p1, p2, p1p2, m1, m2, m3

	double complex xpi(6)
	integer ier

	xpi(1) = m1
	xpi(2) = m2
	xpi(3) = m3
	xpi(4) = p1
	xpi(5) = p2
	xpi(6) = p1p2
	ier = 0
	call ffcc0(CC0, xpi, ier)

	if( ier .gt. MAXDIGITS ) then
	  ier = 0
	  call ffcc0r(CC0, xpi, ier)
	endif

	if( ier .gt. MAXDIGITS ) print *,
     &    "CC0 lost ", ier, " digits (m1 = ", sqrt(dble(m1)), ")"
	end

************************************************************************

	subroutine cc0sub(result, p1, p2, p1p2, m1, m2, m3)
	implicit none
	double complex result
	double complex p1, p2, p1p2, m1, m2, m3

	double complex CC0
	external CC0

	result = CC0(p1, p2, p1p2, m1, m2, m3)
	end

