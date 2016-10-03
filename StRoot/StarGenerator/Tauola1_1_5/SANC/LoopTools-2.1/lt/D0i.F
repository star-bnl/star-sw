* D0i.F
* invocation function for the four-point tensor coefficients
* this file is part of LoopTools
* last modified 16 Jun 04 th

#include "lt.h"


	double complex function D0i(id, p1, p2, p3, p4,
     &    p1p2, p2p3, m1, m2, m3, m4)
	implicit none
	integer id
	double precision p1, p2, p3, p4, p1p2, p2p3
	double precision m1, m2, m3, m4

	double complex Dcache(1)
	common /Dbase/ Dcache

	integer Dget
	external Dget

	D0i = Dcache(
     &    Dget(p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4) + id)
	end

************************************************************************

	subroutine d0isub(result, id, p1, p2, p3, p4,
     &    p1p2, p2p3, m1, m2, m3, m4)
	implicit none
	double complex result
	integer id
	double precision p1, p2, p3, p4, p1p2, p2p3
	double precision m1, m2, m3, m4

	double complex D0i
	external D0i

	result = D0i(id, p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4)
	end

