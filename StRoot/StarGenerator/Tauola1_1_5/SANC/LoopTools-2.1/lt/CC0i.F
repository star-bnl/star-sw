* CC0i.F
* invocation function for the three-point tensor coefficients
* with complex parameters
* this file is part of LoopTools
* last modified 25 Jun 04 th

#include "lt.h"


	double complex function CC0i(id, p1, p2, p1p2, m1, m2, m3)
	implicit none
	integer id
	double complex p1, p2, p1p2, m1, m2, m3

	double complex CCcache(1)
	common /CCbase/ CCcache

	integer CCget
	external CCget

	CC0i = CCcache(CCget(p1, p2, p1p2, m1, m2, m3) + id)
	end

************************************************************************

	subroutine cc0isub(result, id, p1, p2, p1p2, m1, m2, m3)
	implicit none
	double complex result
	integer id
	double complex p1, p2, p1p2, m1, m2, m3

	double complex CC0i
	external CC0i

	result = CC0i(id, p1, p2, p1p2, m1, m2, m3)
	end

