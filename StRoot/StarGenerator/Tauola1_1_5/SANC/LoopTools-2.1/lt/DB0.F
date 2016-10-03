* DB0.F
* the derivative of B0 with respect to p^2
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "lt.h"


	double complex function DB0(p, m1, m2)
	implicit none
	double precision p, m1, m2

	double complex pdb0p

#ifdef CHECK
	external DB0check
#endif

	double precision dm
	integer ier

	dm = (sqrt(m1) - sqrt(m2))**2
	if( p .eq. dm .and. p .ne. 0 .and.
     &      m1 .ne. 0 .and. m2 .ne. 0 ) then
	  pdb0p = (m2 - m1)/2D0/dm*log(m2/m1) - 2
	  DB0 = pdb0p/dm
	else
	  ier = 0
	  call ffxdb0(DB0, pdb0p, p, m1, m2, ier)

	  if( ier .gt. MAXDIGITS ) print *,
     &      "DB0 lost ", ier, " digits (m1 = ", sqrt(m1), ")"
	endif

#ifdef CHECK
	call checkB(DB0, DB0check, "DB0", p, m1, m2)
#endif
	end

************************************************************************

	subroutine db0sub(result, p, m1, m2)
	implicit none
	double complex result
	double precision p, m1, m2

	double complex DB0
	external DB0

	result = DB0(p, m1, m2)
	end

