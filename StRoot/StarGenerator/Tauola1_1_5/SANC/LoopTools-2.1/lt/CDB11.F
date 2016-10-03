* CDB11.F
* the derivative of B11 with respect to p^2 for complex parameters
* this file is part of LoopTools
* last modified 29 Jun 04 th

#include "lt.h"


	double complex function CDB11(p, m1, m2)
	implicit none
	double complex p, m1, m2

	double complex DB11, CA0, CB0, CDB0
	external DB11, CA0, CB0, CDB0

	double complex dm, d2m

	if( dimag(p) .eq. 0 .and.
     &      dimag(m1) .eq. 0 .and. dimag(m2) .eq. 0 ) then
	  CDB11 = DB11(p, m1, m2)
	else
	  dm = (m1 - m2)/p
	  d2m = (m1 - 2*m2)/p

	  CDB11 = 1/3D0*(
     &      ( (.5D0*(m1 + m2) +
     &          (2*dm + 1)*CA0(m1) - (2*dm + 2)*CA0(m2))/p -
     &        (d2m + 2*dm**2)*CB0(p, m1, m2) )/p +
     &      (d2m + dm**2 + 1)*CDB0(p, m1, m2) )
	endif
	end

************************************************************************

	subroutine cdb11sub(result, p, m1, m2)
	implicit none
	double complex result
	double complex p, m1, m2

	double complex CDB11
	external CDB11

	result = CDB11(p, m1, m2)
	end

