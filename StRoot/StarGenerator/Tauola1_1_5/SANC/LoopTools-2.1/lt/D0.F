* D0.F
* the scalar four-point function
* this file is part of LoopTools
* last modified 24 Jun 04 th

#include "lt.h"


	double complex function D0(p1, p2, p3, p4,
     &    p1p2, p2p3, m1, m2, m3, m4)
	implicit none
	double precision p1, p2, p3, p4, p1p2, p2p3
	double precision m1, m2, m3, m4

#ifdef CHECK
	external D0check
#endif

	double precision xpi(13)
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

	if( p1p2 .lt. 0 .and. p2p3 .lt. 0 .and.
     &      p1p2 .gt. p2p3 .and.
     &      m1 .eq. m2 .and. m1 .eq. m3 .and. m1 .eq. m4 ) then
	  xpi(9) = p2p3
	  xpi(10) = p1p2
	endif

	ier = 0
	call ffxd0(D0, xpi, ier)

	if( ier .gt. MAXDIGITS ) then
	  ier = 0
	  call ffxd0r(D0, xpi, ier)
	endif

	if( ier .gt. MAXDIGITS ) print *,
     &    "D0 lost ", ier, " digits (m1 = ", sqrt(m1), ")"

#ifdef CHECK
	call checkD(D0, D0check, "D0",
     &    p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4)
#endif
	end

************************************************************************

	subroutine d0sub(result, p1, p2, p3, p4,
     &    p1p2, p2p3, m1, m2, m3, m4)
	implicit none
	double complex result
	double precision p1, p2, p3, p4, p1p2, p2p3
	double precision m1, m2, m3, m4

	double complex D0
	external D0

	result = D0(p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4)
	end

