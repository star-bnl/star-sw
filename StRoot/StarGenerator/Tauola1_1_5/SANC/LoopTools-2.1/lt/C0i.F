* C0i.F
* invocation function for the three-point tensor coefficients
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "lt.h"


	double complex function C0i(id, p1, p2, p1p2, m1, m2, m3)
	implicit none
	integer id
	double precision p1, p2, p1p2, m1, m2, m3

	double complex Ccache(1)
	common /Cbase/ Ccache

	integer Cget
	external Cget

	C0i = Ccache(Cget(p1, p2, p1p2, m1, m2, m3) + id)
	end

************************************************************************

	subroutine c0isub(result, id, p1, p2, p1p2, m1, m2, m3)
	implicit none
	double complex result
	integer id
	double precision p1, p2, p1p2, m1, m2, m3

	double complex C0i
	external C0i

	result = C0i(id, p1, p2, p1p2, m1, m2, m3)
	end

