* CDB00.F
* the derivative of B00 with respect to p^2 for complex parameters
* this file is part of LoopTools
* last modified 25 Jun 04 th

#include "lt.h"


	double complex function CDB00(p, m1, m2)
	implicit none
	double complex p, m1, m2

	double complex CB1, CDB0, CDB1
	external CB1, CDB0, CDB1

	CDB00 =
     &    1/6D0*(2*m1*CDB0(p, m1, m2) + CB1(p, m1, m2) +
     &      (p + m1 - m2)*CDB1(p, m1, m2)) -
     &    1/18D0
	end

************************************************************************

	subroutine cdb00sub(result, p, m1, m2)
	implicit none
	double complex result
	double complex p, m1, m2

	double complex CDB00
	external CDB00

	result = CDB00(p, m1, m2)
	end

