* CD0i.F
* invocation function for the four-point tensor coefficients
* with complex parameters
* this file is part of LoopTools
* last modified 25 Jun 04 th

#include "lt.h"


	double complex function CD0i(id, p1, p2, p3, p4,
     &    p1p2, p2p3, m1, m2, m3, m4)
	implicit none
	integer id
	double complex p1, p2, p3, p4, p1p2, p2p3
	double complex m1, m2, m3, m4

	double complex CDcache(1)
	common /CDbase/ CDcache

	integer CDget
	external CDget

	CD0i = CDcache(
     &    CDget(p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4) + id)
	end

************************************************************************

	subroutine cd0isub(result, id, p1, p2, p3, p4,
     &    p1p2, p2p3, m1, m2, m3, m4)
	implicit none
	double complex result
	integer id
	double complex p1, p2, p3, p4, p1p2, p2p3
	double complex m1, m2, m3, m4

	double complex CD0i
	external CD0i

	result = CD0i(id, p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4)
	end

