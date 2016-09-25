* CDB1.F
* the derivative of B1 with respect to p^2 for complex parameters
* this file is part of LoopTools
* last modified 29 Jun 04 th

#include "lt.h"


	double complex function CDB1(p, m1, m2)
	implicit none
	double complex p, m1, m2

	double complex DB1, CA0, CB0, CDB0
	external DB1, CA0, CB0, CDB0

	if( dimag(p) .eq. 0 .and.
     &      dimag(m1) .eq. 0 .and. dimag(m2) .eq. 0 ) then
	  CDB1 = DB1(p, m1, m2)
	else
	  CDB1 = .5D0/p*(
     &      (CA0(m2) - CA0(m1) + (m1 - m2)*CB0(p, m1, m2))/p -
     &      (p + m1 - m2)*CDB0(p, m1, m2) )
	endif
	end

************************************************************************

	subroutine cdb1sub(result, p, m1, m2)
	implicit none
	double complex result
	double complex p, m1, m2

	double complex CDB1
	external CDB1

	result = CDB1(p, m1, m2)
	end

