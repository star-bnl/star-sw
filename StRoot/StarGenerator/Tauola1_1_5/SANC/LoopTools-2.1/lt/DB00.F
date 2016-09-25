* DB00.F
* the derivative of B00 with respect to p^2
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "lt.h"


	double complex function DB00(p, m1, m2)
	implicit none
	double precision p, m1, m2

	double complex B1, DB0, DB1
	external B1, DB0, DB1

#ifdef CHECK
	external DB00check
#endif

	DB00 =
     &    1/6D0*(2*m1*DB0(p, m1, m2) + B1(p, m1, m2) +
     &      (p + m1 - m2)*DB1(p, m1, m2)) -
     &    1/18D0

#ifdef CHECK
	call checkB(DB00, DB00check, "DB00", p, m1, m2)
#endif
	end

************************************************************************

	subroutine db00sub(result, p, m1, m2)
	implicit none
	double complex result
	double precision p, m1, m2

	double complex DB00
	external DB00

	result = DB00(p, m1, m2)
	end

